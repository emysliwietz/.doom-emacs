(setq-default
 x-stretch-cursor t)
(good-scroll-mode -1)
(setq-default word-wrap t)

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-margin 2)                            ; It's nice to maintain a little margin

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)

;;; Unicode emojis
(if (>= emacs-major-version 27)
    (set-fontset-font t '(#x1f000 . #x1faff)
                      (font-spec :family "Noto Color Emoji")))
(set-face-attribute
 'default nil :stipple nil :height 120 :width 'normal :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :foundry "outline" :family "Source Code Pro for Powerline")
;;;; setting up composition functions for emoji modifiers
;;(dolist (items `(((?🇦 . ?🇿) [".[🇦-🇿]+" 0 font-shape-gstring])
;                 ((?🏳 . ?🏴) [".[️‍🌈⚧☠󠀠-󠁿]*" 0 font-shape-gstring])
;                 (?⃣ ["[#*0-9]️⃣" 2 font-shape-gstring])
;                 ;; TODO: I can't make keycap sequences work because I
;                 ;; think they're trying to shape with the wrong font.
;                 ,@(mapcar (lambda (range) (list range [".‍?[🏻-🏿]?[‍️♂♀]*️?" 0 font-shape-gstring]))
;                           (concatenate 'list "☝🎅🏇👂👃👦👧👼💏💑💪🕴🕵🕺🖐🖕🖖🙇🚣🛀🛌🤏🤞🤟🤦🤽🤾🥷🦻👯❤"
;                                        '((?⛹ . ?✍) (?🏂 . ?🏄) (?🏊 . ?🏌) (?👆 . ?👐)
;                                          (?👫 . ?👮) (?👰 . ?👸) (?💁 . ?💇) (?🙅 . ?🙇) (?🙋 . ?🙏)
;                                          (?🚴 . ?🚶) (?🤘 . ?🤜) (?🤰 . ?🤹) (?🤼 . ?🤾) (?🦵 . ?🦹)
;                                          (?🧍 . ?🧏) (?🧒 . ?🧟))) )
;                 (?🧑 [".‍?[🏻-🏿]?[‍⚕⚖✈❤️🌾🍳🍼🎄🎓🎤🎨🏫🏭👦-👩💋💻💼🔧🔬🚀🚒🤝🦯🦰-🦳🦼🦽🧑]*" 0 font-shape-gstring])
;                 ((?👨 . ?👩) [".‍?[🏻-🏿]?[‍⚕⚖✈❤️🌾🍳🍼🎄🎓🎤🎨🏫🏭👦-👩💋💻💼🔧🔬🚀🚒🤝🦯🦰-🦳🦼🦽🧑]*" 0 font-shape-gstring])
;                 ,@(mapcar (lambda (str) (list (elt str 0) (vector str 0 'font-shape-gstring)))
;                           '("😶‍🌫️" "🐈‍⬛" "🐕‍🦺" "🐻‍❄️" "👁️‍🗨️" "😮‍💨" "😵‍💫"))))
;  (set-char-table-range
;   composition-function-table
;   (car items)
;   (list (cadr items))))

(setq emojify-emoji-set "twemoji-v2")

(defun emojify--replace-text-with-emoji (orig-fn emoji text buffer start end &optional target)
  "Modify `emojify--propertize-text-for-emoji' to replace ascii/github emoticons with unicode emojis, on the fly."
  (if (or (not emoticon-to-emoji) (= 1 (length text)))
      (funcall orig-fn emoji text buffer start end target)
    (delete-region start end)
    (insert (ht-get emoji "unicode"))))

(define-minor-mode emoticon-to-emoji
  "Write ascii/gh emojis, and have them converted to unicode live."
  :global nil
  :init-value nil
  (if emoticon-to-emoji
      (progn
        (setq-local emojify-emoji-styles '(ascii github unicode))
        (advice-add 'emojify--propertize-text-for-emoji :around #'emojify--replace-text-with-emoji)
        (unless emojify-mode
          (emojify-turn-on-emojify-mode)))
    (setq-local emojify-emoji-styles (default-value 'emojify-emoji-styles))
    (advice-remove 'emojify--propertize-text-for-emoji #'emojify--replace-text-with-emoji)))

(add-hook! '(mu4e-compose-mode org-msg-edit-mode circe-channel-mode org-mode) (emoticon-to-emoji 1))

;;; interface.el -*- lexical-binding: t; -*-

(global-set-key (kbd "<f5>") 'revert-buffer)

                                        ;(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
(global-subword-mode 1)                           ; Iterate through CamelCase words
(setq battery-mode-line-format "%t")

(use-package! selectic-mode
  :commands selectic-mode)

(set-char-table-range composition-function-table ?f '(["\\(?:ff?[fijlt]\\)" 0 font-shape-gstring]))
(set-char-table-range composition-function-table ?T '(["\\(?:Th\\)" 0 font-shape-gstring]))

(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 12
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above
        centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-change-fonts "SourceCodePro" 100))

(defun cleanup-after-init ()
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (kill-unwanted-buffers)
  )

(defun schedule-cleanup-after-init ()
  (run-at-time "1 sec" nil 'cleanup-after-init))

                                        ;(schedule-cleanup-after-init)

                                        ;(add-hook 'after-init-hook 'schedule-cleanup-after-init)

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(use-package! page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'backward-page
        :desc "Next page break" :nv "]" #'forward-page))


(use-package! theme-magic
  :commands theme-magic-from-emacs
  :config
  (defadvice! theme-magic--auto-extract-16-doom-colors ()
    :override #'theme-magic--auto-extract-16-colors
    (list
     (face-attribute 'default :background)
     (doom-color 'error)
     (doom-color 'success)
     (doom-color 'type)
     (doom-color 'keywords)
     (doom-color 'constants)
     (doom-color 'functions)
     (face-attribute 'default :foreground)
     (face-attribute 'shadow :foreground)
     (doom-blend 'base8 'error 0.1)
     (doom-blend 'base8 'success 0.1)
     (doom-blend 'base8 'type 0.1)
     (doom-blend 'base8 'keywords 0.1)
     (doom-blend 'base8 'constants 0.1)
     (doom-blend 'base8 'functions 0.1)
     (face-attribute 'default :foreground))))

(run-with-idle-timer 0.1 nil (lambda () (add-hook 'doom-load-theme-hook 'theme-magic-from-emacs)))

                                        ; Modern org mode
(global-org-modern-mode t)

;; Transparent scratch buffer
(defun buffer-empty-p (&optional buffer)
  (= (buffer-size buffer) 0))

(defvar transparent-mode nil)

(define-minor-mode transparent-mode
  "Toggle default transparency."
  :lighter ""
  :global t
  (if transparent-mode
      (setq transparent-mode nil)
    (setq transparent-mode t))
  (force-mode-line-update))

(defun frame-trans-on ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(0 0)))

(defun frame-trans-off ()
  (interactive)
  (if transparent-mode
      (set-frame-parameter (selected-frame) 'alpha '(95 95))
  (set-frame-parameter (selected-frame) 'alpha '(100 100))))

(defun scratch-trans ()
  (setq my-buffer (get-buffer "*scratch*"))
  (cond ((eq my-buffer (window-buffer (selected-window)))
         (if (= (length (window-list)) 1) (frame-trans-on) (frame-trans-off)))
        ((get-buffer-window my-buffer)
         (frame-trans-off))
        (t
         (frame-trans-off))))

(add-hook 'window-configuration-change-hook 'scratch-trans)

;; Async shell commands without popup buffer
(defun async-shell-command-no-window
    (command)
  "Execute async shell command without popup buffer."
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command command)))
(provide 'interface)
