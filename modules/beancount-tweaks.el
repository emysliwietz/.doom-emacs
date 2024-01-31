;;; beancount.el -*- lexical-binding: t; -*-

(use-package! beancount
  :mode ("\\.beancount\\'" . beancount-mode)
  :init
  (after! all-the-icons
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.beancount\\'" all-the-icons-material "attach_money" :face all-the-icons-lblue))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(beancount-mode all-the-icons-material "attach_money" :face all-the-icons-lblue)))
  :config
  (setq beancount-electric-currency t)
  (defun beancount-bal ()
    "Run bean-report bal."
    (interactive)
    (let ((compilation-read-command nil))
      (beancount--run "bean-report"
                      (file-relative-name buffer-file-name) "bal")))
  (map! :map beancount-mode-map
        :n "TAB" #'beancount-align-to-previous-number
        :i "RET" (cmd! (newline-and-indent) (beancount-align-to-previous-number))))

(defun beancount-select-account ()
  "Select and insert account from current buffer"
  (interactive)
  (with-temp-buffer
    (beancount-open-local)
    (completing-read "Account: " (beancount-collect beancount-account-regexp 0))))

(defun beancount-imported-transaction-change-unknown-account ()
  "Change the Unknown:account field in an imported beancount entry."
  (interactive)
  (save-excursion
    (beancount-goto-transaction-begin)
    (let ((ba (beancount-select-account)))
      (re-search-forward "Unknown:account")
      (replace-match ba)
      ))
  (beancount-finalize-transaction)
  (beancount-goto-next-transaction)
  )

(defun beancount-imported-credit-transaction-change-unknown-account ()
  "Change the Unknown:account field in an imported VR-Visa-Gold beancount entry."
  (interactive)
  (save-excursion
    (beancount-goto-transaction-begin)
    (re-search-forward "Assets:VR-Giro")
    (replace-match "Assets:VR-Visa-Gold")
    )
  (beancount-imported-transaction-change-unknown-account)
  )

(defun beancount-finalize-transaction ()
  "Change transaction marked with * into *"
  (interactive)
  (save-excursion
    (beancount-goto-transaction-begin)
    (re-search-forward "!")
    (replace-match "*")
    )
  )

(defun beancount-transaction-align ()
  "Align beancount transaction."
  (interactive)
  (beancount-align-to-previous-number)
  (beancount-goto-next-transaction)
  )

(defun end-of-buffer-p ()
  "Check if cursor is at the end of the buffer."
  (interactive)
  (= (point) (point-max)))


(defun recenter-middle ()
  "Like recenter-top-bottom, but only centers in middle"
  (interactive)
  (recenter nil t))

; Center screen when going to next or prev transaction
(advice-add '+beancount/next-transaction :after #'recenter-middle)
(advice-add '+beancount/previous-transaction :after #'recenter-middle)

(defun beancount-cleanup-whitespace-in-quotes (str)
  "Replace multiple spaces in STR with a single space, but only between quotes."
  (let ((start 0)
        (res ""))
    (while (string-match "\\(\"[^\"]*\"\\)" str start)
      (let ((match (match-string 0 str)))
        (setq res (concat res
                          (substring str start (match-beginning 0))
                          (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " match)))
        (setq start (match-end 0))))
    (concat res (substring str start))))

(defun beancount-cleanup-buffer ()
  "Cleanup whitespace in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (clean-line (beancount-cleanup-whitespace-in-quotes line)))
        (delete-region (line-beginning-position) (line-end-position))
        (insert clean-line)
        (forward-line 1)))))

(defun beancount-cleanup-hook ()
  "Add `beancount-cleanup-buffer` to `before-save-hook` in Beancount mode."
  (when (eq major-mode 'beancount-mode)
    (add-hook 'before-save-hook #'beancount-cleanup-buffer nil t)))

(add-hook 'beancount-mode-hook #'beancount-cleanup-hook)


  (defun beancount-note-processing-date ()
  "Adds the date the transaction was filed/processed on as note."
  (interactive)
  (save-excursion
    (beancount-goto-transaction-begin)
    (let ((start (point)))
      (evil-forward-WORD-end)
      (forward-char)
      (let ((end (point)))
        (let ((word (buffer-substring start end)))
          (beancount-goto-transaction-end)
          (insert "  processed: \"")
          (insert word)
          (insert "\"\n")
          )))))

(after! beancount
(defun +beancount--navigate-next-transaction ()
  "Move point to beginning of next transaction."
  (interactive)
  ;; make sure we actually move to the next xact, even if we are the beginning
  ;; of one now.
  (if (looking-at +beancount--payee-any-status-regex)
      (forward-line))
  (if (re-search-forward  +beancount--payee-any-status-regex nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max)))
  (if (not (or (string-equal (beancount-get-transaction-type) "*")
               (end-of-buffer-p)))
      (+beancount--navigate-next-transaction)
)))

(defun beancount-get-transaction-type ()
  "Get the type of the current transaction."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) \\([a-z\\*\\!]+\\)")
        (match-string 4)
      )))

(after! beancount
(defun +beancount/sort-region (beg end &optional reverse)
  "Sort the transactions inside BEG and END.
If REVERSE (the prefix arg) is non-nil, sort the transactions in reverst order."
  (interactive
   (list (region-beginning)
         (region-end)
         (and current-prefix-arg t)))
  (let* ((new-beg beg)
         (new-end end)
         (bounds (save-excursion
                   (list (+beancount--navigate-beginning-of-xact)
                         (+beancount--navigate-end-of-xact))))
         (point-delta (- (point) (car bounds)))
         (target-xact (buffer-substring (car bounds) (cadr bounds)))
         (inhibit-modification-hooks t))
    (save-excursion
      (save-restriction
        (goto-char beg)
        ;; make sure beg of region is at the beginning of a line
        (beginning-of-line)
        ;; make sure point is at the beginning of a xact
        (unless (looking-at +beancount--payee-any-status-regex)
          (+beancount--navigate-next-transaction))
        (setq new-beg (point))
        (goto-char end)
        (+beancount--navigate-next-transaction)
        ;; make sure end of region is at the beginning of next record after the
        ;; region
        (setq new-end (point))
        (narrow-to-region new-beg new-end)
        (goto-char new-beg)
        (let ((inhibit-field-text-motion t))
          (sort-subr
           reverse
           #'+beancount--navigate-next-xact
           #'+beancount--navigate-end-of-xact
           #'+beancount--sort-startkey))))
    (goto-char (point-min))
    (re-search-forward (regexp-quote target-xact))
    (goto-char (+ (match-beginning 0) point-delta))))
)


(provide 'beancount-tweaks)
