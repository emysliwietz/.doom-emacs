;;; general.el -*- lexical-binding: t; -*-

(setq mouse-autoselect-window t
      focus-follows-mouse t)

;; Disable backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Delete selection when pasting
(delete-selection-mode 1)

(provide 'general)
