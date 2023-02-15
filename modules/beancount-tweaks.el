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

; install via cargo install beancount-sort
(defun beancount-sort ()
  "Sort default beancount file"
  (interactive)
  (let ((bf (expand-file-name (file-truename (buffer-file-name)))))
    (message bf)
    (when (s-ends-with? ".beancount" bf t)
  (async-shell-command-no-window (format "%sext/bin/beancount-sort --file '%s' --out '%s'" doom-private-dir bf bf)))))



(provide 'beancount-tweaks)
