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
    (message "Exporting to pdf...")
    (org-latex-export-to-pdf t)
    (when (eq major-mode 'latex-mode)
      (force-compile))
    )
  )

(defun org-after-save-cmd ()
  (interactive)
  (when (eq major-mode 'org-mode)
  (let ((cmd (cdr (car (org-collect-keywords '("on_save_cmd"))))))
        (when cmd
  (async-shell-command-no-window (pop cmd))))))
(setq password-cache t ; enable password caching
      password-cache-expiry 36000) ; for ten hours (time in secs)

(add-hook 'after-save-hook 'auto-async-export)
(add-hook 'after-save-hook 'org-after-save-cmd)
                                        ;(global-set-key [f6] (lambda () (interactive) (org-latex-export-to-pdf t)))
(setq org-latex-compiler "lualatex")
(setq-default TeX-master nil)
(add-to-list 'org-latex-packages-alist
             '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

(defun latex-word-count ()
  "Return latex word count using texlive"
  (interactive)
  (let ((file-name (if (eq major-mode 'latex-mode)
                       (buffer-file-name)
                     (if (eq major-mode 'org-mode)
                         (file-name-with-extension (buffer-file-name) "tex")
                       (buffer-file-name)))))
  (message (shell-command-to-string (format "texcount -1 -merge -template={1} %s" file-name)))))

  ;; Latex classes
  (setq org-latex-subtitle-separate t
        org-latex-subtitle-format "\\subtitle{%s}")
  (setq org-latex-classes '(("article" "\\documentclass[a4wide,10pt]{article}"
                             ("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                             ("\\paragraph{%s}" . "\\paragraph*{%s}")
                             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                            ("report" "\\documentclass[11pt]{report}"
                             ("\\part{%s}" . "\\part*{%s}")
                             ("\\chapter{%s}" . "\\chapter*{%s}")
                             ("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                            ("artikel" "\\documentclass[fancy, modern, twocolumn, titlepage=head, paper=a4, 12pt]{artikel}"
                             ("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                             ("\\paragraph{%s}" . "\\paragraph*{%s}")
                             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                            ("thesis" "\\documentclass[fancy, modern, twocolumn, titlepage=thesis, paper=a4, 12pt]{artikel}"
                             ("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                             ("\\paragraph{%s}" . "\\paragraph*{%s}")
                             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                            ("book" "\\documentclass[11pt]{book}"
                             ("\\part{%s}" . "\\part*{%s}")
                             ("\\chapter{%s}" . "\\chapter*{%s}")
                             ("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


(provide 'latex-tweaks)
