;;; org-tweaks.el -*- lexical-binding: t; -*-
;;; Code:
(after! org
  (ifdirexists "~/sync/org/"
               (setq org-directory dir))
  (ifdirexists "~/sync/agenda"
               (setq org-agenda-files (directory-files "~/sync/agenda/" t (rx ".org" eos))))
  (setq org-todo-keywords '((sequence "TODO(t)" "LECT(l)" "EXAM(e)" "MEET(m)" "PROJ(p)" "LOOP(L)" "START(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "INPRO(n)" "OPT(o)" "READ(r)" "|" "DONE(d)" "KILL(k)")
                            (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
                            (sequence "|" "OKAY(O)" "YES(Y)" "NO(N)"))
        org-startup-folded t
        org-log-done 'time
        org-log-reschedule 'time
        initial-major-mode 'org-mode
        org-export-async-init-file "/home/user/.doom.d/ext/export/org-export-init.el"
        org-latex-src-block-backend 'engraved)

  ;;; Add org mode to txt and archive files
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

  ;; Org Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-auto-tangle-default t)
  ;; Auto tangle
  (add-hook 'org-mode-hook 'org-auto-tangle-mode)
)

(setq beancount-main-file "/media/user/keychain/finances/wallet.beancount")
;; Capture
(setq org-default-notes-file "~/dox/notes/notes.org")
(setq org-capture-templates
      '(("b" "Beancount Entry" plain
         (file org-default-notes-file)
         ;"%(progn (yas-expand-snippet (yas-lookup-snippet \"beancount\" 'org-mode)) nil)")))
         "bc")))

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
