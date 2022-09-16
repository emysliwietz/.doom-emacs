;;; org-tweaks.el -*- lexical-binding: t; -*-

(after! org
  (setq org-directory "~/sync/org/"
        org-agenda-files (directory-files "~/sync/agenda/" t directory-files-no-dot-files-regexp)
        org-todo-keywords '((sequence "TODO(t)" "LECT(l)" "MEET(m)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
                            (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
                            (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))
        org-startup-folded t
        org-log-done 'time
        org-log-reschedule 'time)

  ;;; Add org mode to txt and archive files
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
)

(provide 'org-tweaks)
