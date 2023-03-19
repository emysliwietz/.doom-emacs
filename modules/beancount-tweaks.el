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
    (replace-match "Assets:VR-Credit-Gold")
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

(provide 'beancount-tweaks)
