(defun elfeed-summary-load-async-update ()
  "Asynchronously update the database, load it and update summary buffer."
  (interactive)
  (async-start-process "Elfeed Update" "emacs" '(lambda (name)
                                                  (elfeed-db-load)
                                        ;(elfeed-summary-update)
                                                  (message "-> %s completed!" name))
                       "--script" (concat doom-user-dir "ext/elfeed/update.el")))
