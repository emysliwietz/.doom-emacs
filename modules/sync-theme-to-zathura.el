(setq zathura-set-theme-command "~/.scripts/zathura-recolor.sh")

(defvar enables-sync-theme-to-zathura t)

(defun sync-theme-to-zathura ()
  (interactive)
  (when enables-sync-theme-to-zathura
   (let ((bg (face-attribute 'default :background))
     (fg (face-attribute 'default :foreground))
     (mbg (face-attribute 'mode-line :background))
     (mfg (face-attribute 'mode-line :foreground)))
     (call-process zathura-set-theme-command nil nil nil fg bg fg bg))))

(defun consult-theme--maybe-sync-to-zathura (theme)
  "Disable current themes and enable THEME from `consult-themes'.

The command supports previewing the currently selected theme."
  (interactive
   (list
    (let* ((regexp (consult--regexp-filter
                    (mapcar (lambda (x) (if (stringp x) x (format "\\`%s\\'" x)))
                            consult-themes)))
           (avail-themes (seq-filter
                          (lambda (x) (string-match-p regexp (symbol-name x)))
                          (cons 'default (custom-available-themes))))
           (saved-theme (car custom-enabled-themes)))
      (consult--read
       (mapcar #'symbol-name avail-themes)
       :prompt "Theme: "
       :require-match t
       :category 'theme
       :history 'consult--theme-history
       :lookup (lambda (selected &rest _)
                 (setq selected (and selected (intern-soft selected)))
                 (or (and selected (car (memq selected avail-themes)))
                     saved-theme))
       :state (lambda (action theme)
                (pcase action
                  ('return (consult-theme (or theme saved-theme)))
                  ((and 'preview (guard theme)) (consult-theme theme))))
       :default (symbol-name (or saved-theme 'default))))))
  (when (eq theme 'default) (setq theme nil))
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (progn (enable-theme theme) (sync-theme-to-zathura))
        (progn (load-theme theme :no-confirm)
           (sync-theme-to-zathura))))))

(add-function :override (symbol-function 'consult-theme) #'consult-theme--maybe-sync-to-zathura)

(provide 'sync-theme-to-zathura)
