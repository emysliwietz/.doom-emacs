;;; org-tweaks.el -*- lexical-binding: t; -*-
;;; Code:
(after! org
  (ifdirexists "~/sync/org/"
               (setq org-directory dir))
  (ifdirexists "~/sync/agenda"
               (setq org-agenda-files (directory-files "~/sync/agenda/" t (rx ".org" eos))))
  (setq org-todo-keywords '((sequence "TODO(t)" "LECT(l)" "EXAM(e)" "MEET(m)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
                            (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
                            (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))
        org-startup-folded t
        org-log-done 'time
        org-log-reschedule 'time
        initial-major-mode 'org-mode
        org-export-async-init-file "/home/user/.doom.d/ext/export/org-export-init.el"
        org-latex-src-block-backend 'engraved
        TeX-command-extra-options "--shell-escape")

  ;;; Add org mode to txt and archive files
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

  ;; Org Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-auto-tangle-default t)
  ;; Auto tangle
  (add-hook 'org-mode-hook 'org-auto-tangle-mode)

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
)

(defun org-agenda-export-to-ics ()
  "Exports current org agenda buffer to ics, treating DEADLINES as dates"
  (interactive)
  (with-temp-buffer
    (cl-map 'nil #'insert-file-contents org-agenda-files)
    (replace-regexp-entire-buffer "<.*> \\(<.*>\\)" "\\1")
    (replace-regexp-entire-buffer "\\(<.*>\\) <.*>" "\\1")
    (replace-regexp-entire-buffer "SCHEDULED: \\(<.*>\\)" "\\1")
    (replace-regexp-entire-buffer "DEADLINE: \\(<.*>\\)" "\\1")
    (message (org-icalendar-export-to-ics))))


(provide 'org-tweaks)
