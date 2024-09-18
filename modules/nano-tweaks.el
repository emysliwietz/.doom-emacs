(require 'nano-base-colors)
(require 'nano-faces)
(require 'nano-help)
(load-module 'nano-sidebar)
(load-module 'nano-mu4e)

(defun mu4e-headers-sender-subject (msg)
  (let* ((thread  (mu4e-message-field msg :thread))
         (level (plist-get thread :level))
         (empty-parent (and thread (plist-get thread :empty-parent)))
         (child   (and thread (> (plist-get thread :level) 0)))
         (flagged (memq 'flagged  (mu4e-message-field msg :flags)))
         (unread  (memq 'unread  (mu4e-message-field msg :flags)))
         (replied (memq 'replied (mu4e-message-field msg :flags)))
         (maildir (mu4e-get-maildir msg))
         (inbox   (string= maildir "inbox"))
         (attachment (memq 'attach (mu4e-message-field msg :flags)))
         (sent    (string= maildir "sent"))
         (tags    (mu4e-message-field msg :tags))
         (list    (mu4e-message-field msg :mailing-list))
         (sender-name     (or (mu4e-message-field (car (mu4e-message-field msg :from)) :name)))
         (sender-address  (or (car (cdr (car (mu4e-message-field msg :from))))))
         (subject (mu4e-message-field msg :subject)))
    (setq messagemsg msg)
    (concat
     (if (and (not empty-parent) child)
     ;; (if (> level 0)
         (propertize "│ " 'face `(:foreground ,nano-color-subtle :height 100)))

     (mu4e-headers-button
      (string-pad (or sender-name sender-address "Unknown") 40)
      (cond  ((and child unread) 'nano-face-default)
             (child              'nano-face-faded)
             (unread             'nano-face-strong)
             (t                  'nano-face-strong))

      (format "Mails from %s" sender-address)
      (format "from:%s" sender-address))

     (cond (unread ;;(propertize " ● " 'face 'nano-face-default))
            (propertize "  " 'face '(:height 100)))
           (inbox
            (propertize " • " 'face 'nano-face-faded))
           ((or (not child) empty-parent tags)
            (propertize "  " 'face 'nano-face-faded)))


     (if (or (not mu4e-headers-show-threads) (not child))
         (if list (concat (mu4e-headers-button
                           "[LIST]"
                           '(:inherit nano-face-faded)
                           (format "Mails from/to %s" list)
                           (format "list:%s" list)) " ")))

     (if tags (concat
               (mapconcat
                (function (lambda (tag)
                            (mu4e-headers-button
                             (format "[%s]" tag)
                             '(:inherit (nano-face-salient nano-face-strong))
                             (format "Mails with tag %s" tag)
                             (concat "tag:" tag))))  tags " ") " "))

     (if (or (not child) empty-parent)
     ;; (if (= level 0)
         (propertize subject 'face 'nano-face-default)))))

(keymapp mu4e-headers-mode-map)
(keymapp "f")


(defun mu4e-headers-button (text face help query)
  "Make a clickable button with specified FACE displaying TEXT.
   When hovered, HELP is displayed. When clicked, mu4e QUERY is executed."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mu4e-headers-mode-map)
    (define-key map [mouse-1] `(lambda ()
                                 (interactive) (mu4e-headers-search ,query)))
    (propertize text
                'face face
                'mouse-face `(:foreground ,nano-color-background
                              :background ,nano-color-faded)
                'local-map map
                'help-echo `(lambda (window _object _point)
                              (let (message-log-max) (message ,help))))))

(defun mu4e-headers-date-button (date face)
  (concat
 (mu4e-headers-button (format-time-string "%Y" date)
                        face
                        (format-time-string "Mails from %Y" date)
                        (format-time-string "date:%Y" date))
   (propertize "/" 'face face)
   (mu4e-headers-button (format-time-string "%m" date)
                        face
                        (format-time-string "Mails from %B %Y" date)
                        (format-time-string "date:%Y%m" date))
   (propertize "/" 'face face)
   (mu4e-headers-button (format-time-string "%d" date)
                        face
                        (format-time-string "Mails from %d %B %Y" date)
                        (format-time-string "date:%Y%m%d" date))
     ))

;; (setq package-install-upgrade-built-in t)
;; (setq nano-font-size 10
;;       nano-font-family-monospaced "Source Code Pro Semibold"
;;       nano-modeline-position 'mode-line
;;       nano-modeline-style '(3d accented)
;;       doom-one-brighter-comments t)

;; ;;Default layout (optional)
;; (require 'nano-layout)

;; ;; Theme
;; (require 'nano-faces)
;; (require 'nano-theme)
;; (require 'nano-theme-dark)
;; (require 'nano-theme-light)

;; (set-face-attribute 'default nil :family "Source Code Pro" :weight 'semibold)
;; (set-face-attribute 'bold nil :family "Source Code Pro" :weight 'black)
;; (require 'nano-writer)

;; (nano-theme-set-dark)
;; (call-interactively 'nano-refresh-theme)
;; (load-theme doom-theme)

;; ;; Nano session saving (optional)
;; ;(require 'nano-session)

;; ;; Nano header & mode lines (optional)
;; ;(require 'nano-modeline)

;; ;(setq widget-image-enable nil)
(provide 'nano-tweaks)
