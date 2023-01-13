;;; latex.el -*- lexical-binding: t; -*-

(setq TeX-command-extra-options "--shell-escape")
(setq TeX-engine 'luatex)
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "<f6>")
                           (lambda () (interactive) (org-latex-export-to-pdf t)))))

                                        ;(global-set-key [f6] (lambda () (interactive) (org-latex-export-to-pdf t)))
(setq org-latex-compiler "lualatex")
(setq-default TeX-master nil)
(add-to-list 'org-latex-packages-alist
             '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

(provide 'latex-tweaks)
