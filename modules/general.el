;;; general.el -*- lexical-binding: t; -*-

(setq mouse-autoselect-window t
      focus-follows-mouse t)

;; Disable backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Delete selection when pasting
(delete-selection-mode 1)

;; Save the session
(desktop-save-mode 1)
(setq desktop-restore-eager 10)
;; Save last visited place in files
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/etc/saveplace")

(provide 'general)
