(setq +org-capture-todo-file "/home/user/sync/agenda/todo.org"
      +org-capture-notes-file "/home/user/sync/agenda/notes.org"
      +org-capture-journal-file "/home/user/sync/agenda/journal.org")

; Handled by Doom Emacs
  ;; (add-to-list 'org-capture-templates
  ;;              '("Ö" "Todo" entry ; key, name, type
  ;;                (file+olp+datetree +org-capture-todo-file) ; target
  ;;                "* TODO %^{Title}\n%T %?"
  ;;                :prepend t ; properties
  ;;                :kill-buffer t))

  ;; (add-to-list 'org-capture-templates
  ;;              '("Ä" "Note" entry ; key, name, type
  ;;                (file+olp+datetree +org-capture-notes-file) ; target
  ;;                "* %^{Title} %^G\n%T %?"
  ;;                :prepend t ; properties
  ;;                :kill-buffer t))


(provide 'org-capture-tweaks)
