;;; read-single.el -*- lexical-binding: t; -*-

(use-package! spray
  :commands spray-mode
  :config
  (setq spray-wpm 400
        spray-height 300)
  (defun spray-mode-hide-cursor ()
    "Hide or unhide the cursor as is appropriate."
    (if spray-mode
        (setq-local spray--last-evil-cursor-state evil-normal-state-cursor
                    evil-normal-state-cursor '(nil))
      (setq-local evil-normal-state-cursor spray--last-evil-cursor-state)))
  (add-hook 'spray-mode-hook #'spray-mode-hide-cursor)
  (map! :map spray-mode-map
        :n "<return>" #'spray-start/stop
        :n "SPC" #'spray-start/stop
        :n "f" #'spray-faster
        :n "s" #'spray-slower
        :n "t" #'spray-time
        :n "<right>" #'spray-forward-word
        :n "h" #'spray-forward-word
        :n "<left>" #'spray-backward-word
        :n "l" #'spray-backward-word
        :n "q" #'spray-quit))

(provide 'speed-read)
