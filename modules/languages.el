;;; languages.el -*- lexical-binding: t; -*-

;; CANoe/CAPL
(define-derived-mode capl-mode
  c-mode "CAPL"
  "Major mode for CANoe/CAPL."
  (flycheck-mode 0))

;; Rust
(setq rustic-format-trigger 'on-save
      rustic-format-on-save t)

(add-hook! 'prog-mode-hook 'which-function-mode)
(setq which-func-display 'header)
;; Latin
;;

;;;###autoload
(define-minor-mode latin-minor-mode
  "Minor mode for writing Church Latin"
  :lighter " ☧"
  :global t)

(defun latin-minor-mode--insert-ae ()
  "Replace ae with æ"
  (interactive)
  (if (bound-and-true-p latin-minor-mode)
  (if (eq (char-before) ?a)
      (progn
        (backward-delete-char 1)
        (insert "æ"))
    (if (eq (char-before) ?A)
        (progn
          (backward-delete-char 1)
          (insert "Æ")
          )
      (insert "e")))
  (self-insert-command 1)))

(defun latin-minor-mode--insert-versicle ()
  "Replace VV with ℣"
  (interactive)
  (if (bound-and-true-p latin-minor-mode)
  (if (eq (char-before) ?V)
      (progn
        (backward-delete-char 1)
        (insert "℣"))
      (insert "V"))
  (self-insert-command 1)))

(defun latin-minor-mode--insert-response ()
  "Replace RR with ℟"
  (interactive)
  (if (bound-and-true-p latin-minor-mode)
  (if (eq (char-before) ?R)
      (progn
        (backward-delete-char 1)
        (insert "℟"))
      (insert "R"))
  (self-insert-command 1)))



(map! :map latin-minor-mode-map
      :n "e" #'latin-minor-mode--insert-ae
      :n "R" #'latin-minor-mode--insert-response
      :n "V" #'latin-minor-mode--insert-versicle)

;; For some reason, that doesn't work...
;;(progn
;;  (global-set-key (kbd "e") 'latin-minor-mode--insert-ae)
;;  (global-set-key (kbd "R") 'latin-minor-mode--insert-response)
;;  (global-set-key (kbd "V") 'latin-minor-mode--insert-versicle))
(provide 'languages)
