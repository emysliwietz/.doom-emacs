;;; popes.el -*- lexical-binding: t; -*-

(defun get-pope-image ()
  (let* (
         (folder (concat doom-private-dir "/scripts/popes/images/"))
         (files (directory-files folder nil "\\.png\\'"))
         (number-popes (length files))
         (pope-img (nth (random number-popes) files)))
    (setq currently-displayed-pope (replace-regexp-in-string ".png" "" pope-img))
    (setq pope-info (shell-command-to-string (concat "grep -m1 \"" currently-displayed-pope "\" " folder "../pope_info.txt")))
    (concat folder pope-img)
    )
  )


(defun psalm ()
  (shell-command-to-string "~/.scripts/psalms.sh de"))


(setq fancy-splash-last-size nil)
(setq fancy-splash-last-theme nil)
(defun set-appropriate-splash (&rest _)
   (setq fancy-splash-image (get-pope-image))
   (setq +doom-dashboard-banner-padding '(5 . 5))
   (setq fancy-splash-last-theme doom-theme)
   (+doom-dashboard-reload))

(add-hook 'window-size-change-functions #'set-appropriate-splash)
(add-hook 'doom-load-theme-hook #'set-appropriate-splash)

(defun doom-dashboard-phrase ()
  "Get a splash phrase, flow it over multiple lines as needed, and make fontify it."
  (mapconcat
   (lambda (line)
     (+doom-dashboard--center
      +doom-dashboard--width
      (with-temp-buffer
        (insert-text-button
         line
         'action
         (lambda (_) (+doom-dashboard-reload t))
         'face 'doom-dashboard-menu-title
         'mouse-face 'doom-dashboard-menu-title
         'help-echo currently-displayed-pope
         'follow-link t)
        (buffer-string))))
   (split-string
    (with-temp-buffer
      (insert (concat "Seine Heiligkeit " currently-displayed-pope "\n\n" pope-info "\n" "Heiliger Vater, bete für uns." "\n\n"))
      (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
      (fill-region (point-min) (point-max))
      (buffer-string))
    "\n")
   "\n"))

(defadvice! doom-dashboard-widget-loaded-with-phrase ()
  :override #'doom-dashboard-widget-loaded
  (setq line-spacing 0.2)
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (doom-display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"
   (doom-dashboard-phrase)
   (psalm)
   "\n"))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(provide 'popes)
