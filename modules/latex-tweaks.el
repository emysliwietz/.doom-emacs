;;; latex.el -*- lexical-binding: t; -*-

(setq TeX-command-extra-options "--shell-escape")
(setq TeX-engine 'luatex)
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "<f6>")
                           (lambda () (interactive) (org-latex-export-to-pdf t)))))
(defun force-compile ()
  "Set the file modification times on the current file, then call
TeX-command-sequence.
This forces a complete recompilation of the document, even if the source
(.tex) is older than any existing outputs (.pdf etc)."
  (interactive)
  (set-buffer-modified-p t) ;; sets mod time to current time
  (save-buffer)
  (TeX-command-sequence t t))

(defun auto-async-export ()
  (when (and (eq major-mode 'org-mode)
             (string-equal "t" (pop (cdr (car (org-collect-keywords '("auto_async_export")))))))
    (org-latex-export-to-pdf t)
    (when (eq major-mode 'latex-mode)
      (force-compile))
    )
  )
(add-hook! 'after-save-hook 'auto-async-export)
                                        ;(global-set-key [f6] (lambda () (interactive) (org-latex-export-to-pdf t)))
(setq org-latex-compiler "lualatex")
(setq-default TeX-master nil)
(add-to-list 'org-latex-packages-alist
             '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

(provide 'latex-tweaks)
