;;; org-tweaks.el -*- lexical-binding: t; -*-

(after! org
  (setq org-directory "~/sync/org/"
        org-agenda-files (directory-files "~/sync/agenda/" t (rx ".org" eos))
        org-todo-keywords '((sequence "TODO(t)" "LECT(l)" "EXAM(e)" "MEET(m)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
                            (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
                            (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))
        org-startup-folded t
        org-log-done 'time
        org-log-reschedule 'time
        initial-major-mode 'org-mode
        org-export-async-init-file "/home/user/.doom.d/ext/export/org-export-init.el"
        org-latex-src-block-backend 'engraved
        org-latex-compiler "xelatex")

  ;;; Add org mode to txt and archive files
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

  ;; Latex classes
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
                            ("book" "\\documentclass[11pt]{book}"
                             ("\\part{%s}" . "\\part*{%s}")
                             ("\\chapter{%s}" . "\\chapter*{%s}")
                             ("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
)
(provide 'org-tweaks)
