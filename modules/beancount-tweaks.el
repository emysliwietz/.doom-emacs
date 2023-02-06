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

(defun get-matching-groups-from-file (filename regex)
  "Return a list of all matching groups for REGEX in the file specified by FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (let (matches)
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let ((match (match-string-no-properties 1)))
          (push match matches)))
      matches)))

(defun ivy-dmenu (list)
  "Select an item from LIST using ivy and insert it into the current buffer."
  (ivy-read "Select item: " list))


(defun beancount-select-account ()
  "Select and insert expense account"
  (interactive)
  (ivy-dmenu (get-matching-groups-from-file "/media/user/keychain/finances/wallet.beancount" ".* open \\(Expenses:.*\\)")))

(provide 'beancount-tweaks)
