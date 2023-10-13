(setq home-hostname-alist '("astaroth" "jarvis")
      home? (member (system-name) home-hostname-alist))

(require 'string-inflection)
(defun string-inflection-title-to-lisp-case-function (title-str)
  "Title String for Something => title-string-for-something"
  (string-inflection-kebab-case-function (s-replace-all '((" " . "-")) title-str)))

;;; Sometimes exwm fails to sets a buffer, so set it to scratch
;;; with a button press
(defun go-to-scratch ()
  (interactive)
  (message "%s" (selected-window))
  (message (format "Class: %s" exwm-class-name))
  (message (format "Instance: %s" exwm-instance-name))
  (message (format "Title: %s" exwm-title))
  ;(message "%s" (exwm-class-name (selected-window)))
  (switch-to-buffer "*scratch*"))

(defun go-to-scratch-other ()
  (interactive)
  (switch-to-buffer-other-frame "*scratch*"))

(setq save-temp-location "~/dox/temp-save/")
(defun save-buffer-temp ()
  (interactive)
  (let* ((s (buffer-string))
         (ss (split-string s " "))
         (nl (butlast ss (- (length ss) 5)))

         )
    (set-visited-file-name (concat save-temp-location (mapconcat '(lambda (x)  (format "%s" x))  nl " ") ".org"))
    (save-buffer)
    )
  )

  (defun switchmonitor-next ()
    (interactive)
    (shell-command "xdotool mousemove_relative 1920 0"))

  (defun switchmonitor-prev ()
    (interactive)
    (shell-command "xdotool mousemove_relative -- -1920 0"))

(defmacro ifdirexists (dir &rest actions)
  "Execute functions taking dir as an argument if dir exists"
  `(when (file-exists-p ,dir)
     ((lambda (dir)
       ,@actions) ,dir)))

(provide 'utility-functions)
