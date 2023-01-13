;;; exwm-tweaks.el -*- lexical-binding: t; -*-
(use-package! exwm
  :config
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-default)
  (require 'exwm-randr)

(when (string= (system-name) "astaroth")
  (setq exwm-randr-workspace-output-plist '(1 "DP-2-1" 2 "HDMI-2" 3 "DP-2-2" 4 "eDP-1")))
(when (string= (system-name) "jarvis")
  (setq exwm-randr-workspace-output-plist '(1 "DisplayPort-0" 2 "DVI-0" 3 "HDMI-0" 4 "eDP-1")))

  (add-hook 'exwm-randr-screen-change-hook
	    (lambda ()
	      (start-process-shell-command
	       "xrandr" nil "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output DP-1 --off --output HDMI-1 --off --output DP-2 --off --output HDMI-2 --mode 1920x1080 --pos 0x0 --rotate normal")))

  (exwm-randr-enable)
  (winner-mode t)
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (define-key exwm-mode-map (kbd "C-c") nil)
  (setq exwm-input-simulation-keys
	'(([?\C-b] . [left])
	  ([?\C-f] . [right])
	  ([?\C-p] . [up])
	  ([?\C-n] . [down])
	  ([?\C-a] . [home])
	  ([?\C-e] . [end])
	  ([?\M-a] . [C-a])
	  ([?\M-v] . [prior])
	  ([?\C-d] . [delete])
	  ([?\C-k] . [S-end delete])
	  ([?\C-w] . [?\C-x])
	  ([?\M-w] . [?\C-c])
	  ([?\C-y] . [?\C-v])
	  ;; search
	  ([?\C-s] . [?\C-f])
	  ([?\M-s] . [?\C-s])))
  (when (functionp 'exwm-enable-ido-workaround)
    (exwm-enable-ido-workaround))
  (with-eval-after-load 'ediff-wind
  (setq ediff-control-frame-parameters
	(cons '(unsplittable . t) ediff-control-frame-parameters)))

  (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
;  (global-set-key (kbd "C-c m") 'toggle-maximize-buffer)


  (defun fullscreen ()
    (interactive)
    (if (eq major-mode 'exwm-mode)
      (call-interactively 'exwm-layout-toggle-fullscreen)
      (toggle-maximize-buffer)
      ))

;;; Make current buffer float
(defun toggle-float-buffer ()
  (interactive)
  (if (eq major-mode 'exwm-mode)
      (progn
      (call-interactively 'exwm-floating-toggle-floating)
      (call-interactively 'exwm-layout-hide-mode-line)
      )))


;;; Sometimes exwm fails to sets a buffer, so set it to scratch
;;; with a button press
(defun go-to-scratch ()
  (interactive)
  (message "%s" (selected-window))
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


  (setq exwm-workspace-number 9
        exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t
        exwm-manage-force-tiling t)
  (setq exwm-input-global-keys
      `(([?\s-f] . fullscreen)
	([?\s-F] . toggle-maximize-buffer)
        ([?\s-g] . toggle-float-buffer)
	([?\s-q] . kill-curr-buffer)
	([?\s-n] . switchmonitor-next)
	([?\s-p] . switchmonitor-prev)
        ;((kbd "s-<return>") . switchmonitor-prev)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))
  (add-hook 'exwm-manage-finish-hook
          (lambda ()
            (if (and exwm-class-name
                       (string= exwm-class-name "St"))
              (progn
		(exwm-input-release-keyboard))
	      (progn))
	    (exwm-layout-hide-mode-line)))

(setq exwm-input-prefix-keys 
'(?\C-x ?\C-u ?\C-h ?\M-x ?\M-` ?\M-& ?\M-:))

 
(global-set-key (kbd "s-<f4>") 'go-to-scratch)
(global-set-key (kbd "s-S-<f4>") 'save-buffer-temp)
(require 'exwm-edit)
(defun ag-exwm/on-exwm-edit-compose ()
  (funcall 'org-mode))
(add-hook 'exwm-edit-compose-hook 'ag-exwm/on-exwm-edit-compose)


(add-hook 'exwm-update-title-hook
          (lambda ()
              (exwm-workspace-rename-buffer exwm-title))))

(setq exwm-manage-configurations
      '(((or (string-equal exwm-class-name "Nm-applet")
             (string-equal exwm-class-name "Surf")
             (string-equal exwm-class-name "Steam")
             (not (message exwm-class-name)))
           floating t
           floating-mode-line nil
;           width 0.4
;           height 0.4
	   )
        ((equal exwm-window-type xcb:Atom:_NET_WM_WINDOW_TYPE_DIALOG)
         floating t
         floating-mode-line nil)
        ))

(defun exwm-floating--set-floating (id)
  "Make window ID floating."
  (let ((window (get-buffer-window (exwm--id->buffer id))))
    (when window
      ;; Hide the non-floating X window first.
      (set-window-buffer window (other-buffer nil t))))
  (let* ((original-frame (buffer-local-value 'exwm--frame
                                             (exwm--id->buffer id)))
         ;; Create new frame
         (frame (with-current-buffer
                    (or (get-buffer "*scratch*")
                        (progn
                          (set-buffer-major-mode
                           (get-buffer-create "*scratch*"))
                          (get-buffer "*scratch*")))
                  (make-frame
                   `((minibuffer . ,(minibuffer-window exwm--frame))
                     (left . ,(* window-min-width -10000))
                     (top . ,(* window-min-height -10000))
                     (width . ,window-min-width)
                     (height . ,window-min-height)
                     (unsplittable . t))))) ;and fix the size later
         (outer-id (string-to-number (frame-parameter frame 'outer-window-id)))
         (window-id (string-to-number (frame-parameter frame 'window-id)))
         (frame-container (xcb:generate-id exwm--connection))
         (window (frame-first-window frame)) ;and it's the only window
         (x (slot-value exwm--geometry 'x))
         (y (slot-value exwm--geometry 'y))
         (width (slot-value exwm--geometry 'width))
         (height (slot-value exwm--geometry 'height)))
    ;; Force drawing menu-bar & tool-bar.
    (redisplay t)
    (exwm-workspace--update-offsets)
    (exwm--log "Floating geometry (original): %dx%d%+d%+d" width height x y)
    ;; Save frame parameters.
    (set-frame-parameter frame 'exwm-outer-id outer-id)
    (set-frame-parameter frame 'exwm-id window-id)
    (set-frame-parameter frame 'exwm-container frame-container)
    (set-frame-parameter frame 'alpha 10)
    ;; Fix illegal parameters
    ;; FIXME: check normal hints restrictions
    (let* ((workarea (elt exwm-workspace--workareas
                          (exwm-workspace--position original-frame)))
           (x* (aref workarea 0))
           (y* (aref workarea 1))
           (width* (aref workarea 2))
           (height* (aref workarea 3)))
      ;; Center floating windows
      (when (and (or (= x 0) (= x x*))
                 (or (= y 0) (= y y*)))
        (let ((buffer (exwm--id->buffer exwm-transient-for))
              window edges)
          (when (and buffer (setq window (get-buffer-window buffer)))
            (setq edges (window-inside-absolute-pixel-edges window))
            (unless (and (<= width (- (elt edges 2) (elt edges 0)))
                         (<= height (- (elt edges 3) (elt edges 1))))
              (setq edges nil)))
          (if edges
              ;; Put at the center of leading window
              (setq x (+ x* (/ (- (elt edges 2) (elt edges 0) width) 2))
                    y (+ y* (/ (- (elt edges 3) (elt edges 1) height) 2)))
            ;; Put at the center of screen
            (setq x (/ (- width* width) 2)
                  y (/ (- height* height) 2)))))
      (if (> width width*)
          ;; Too wide
          (progn (setq x x*
                       width width*))
        ;; Invalid width
        (when (= 0 width) (setq width (/ width* 2)))
        ;; Make sure at least half of the window is visible
        (unless (< x* (+ x (/ width 2)) (+ x* width*))
          (setq x (+ x* (/ (- width* width) 2)))))
      (if (> height height*)
          ;; Too tall
          (setq y y*
                height height*)
        ;; Invalid height
        (when (= 0 height) (setq height (/ height* 2)))
        ;; Make sure at least half of the window is visible
        (unless (< y* (+ y (/ height 2)) (+ y* height*))
          (setq y (+ y* (/ (- height* height) 2)))))
      ;; The geometry can be overridden by user options.
      (let ((x** (plist-get exwm--configurations 'x))
            (y** (plist-get exwm--configurations 'y))
            (width** (plist-get exwm--configurations 'width))
            (height** (plist-get exwm--configurations 'height)))
        (if (integerp x**)
            (setq x (+ x* x**))
          (when (and (floatp x**)
                     (>= 1 x** 0))
            (setq x (+ x* (round (* x** width*))))))
        (if (integerp y**)
            (setq y (+ y* y**))
          (when (and (floatp y**)
                     (>= 1 y** 0))
            (setq y (+ y* (round (* y** height*))))))
        (if (integerp width**)
            (setq width width**)
          (when (and (floatp width**)
                     (> 1 width** 0))
            (setq width (max 1 (round (* width** width*))))))
        (if (integerp height**)
            (setq height height**)
          (when (and (floatp height**)
                     (> 1 height** 0))
            (setq height (max 1 (round (* height** height*))))))))
    (exwm--set-geometry id x y nil nil)
    (xcb:flush exwm--connection)
    (exwm--log "Floating geometry (corrected): %dx%d%+d%+d" width height x y)
    ;; Fit frame to client
    ;; It seems we have to make the frame invisible in order to resize it
    ;; timely.
    ;; The frame will be made visible by `select-frame-set-input-focus'.
    (make-frame-invisible frame)
    (let* ((edges (window-inside-pixel-edges window))
           (frame-width (+ width (- (frame-pixel-width frame)
                                    (- (elt edges 2) (elt edges 0)))))
           (frame-height (+ height (- (frame-pixel-height frame)
                                      (- (elt edges 3) (elt edges 1)))
                            ;; Use `frame-outer-height' in the future.
                            exwm-workspace--frame-y-offset))
           (floating-mode-line (plist-get exwm--configurations
                                          'floating-mode-line))
           (floating-header-line (plist-get exwm--configurations
                                            'floating-header-line))
           (border-pixel (exwm--color->pixel exwm-floating-border-color)))
      (if floating-mode-line
          (setq exwm--mode-line-format (or exwm--mode-line-format
                                           mode-line-format)
                mode-line-format floating-mode-line)
        (if (and (not (plist-member exwm--configurations 'floating-mode-line))
                 exwm--mwm-hints-decorations)
            (when exwm--mode-line-format
              (setq mode-line-format exwm--mode-line-format))
          ;; The mode-line need to be hidden in floating mode.
          (setq frame-height (- frame-height (window-mode-line-height
                                              (frame-root-window frame)))
                exwm--mode-line-format (or exwm--mode-line-format
                                           mode-line-format)
                mode-line-format nil)))
      (if floating-header-line
          (setq header-line-format floating-header-line)
        (if (and (not (plist-member exwm--configurations
                                    'floating-header-line))
                 exwm--mwm-hints-decorations)
            (setq header-line-format nil)
          ;; The header-line need to be hidden in floating mode.
          (setq frame-height (- frame-height (window-header-line-height
                                              (frame-root-window frame)))
                header-line-format nil)))
      (set-frame-size frame frame-width frame-height t)
      ;; Create the frame container as the parent of the frame.
      (xcb:+request exwm--connection
          (make-instance 'xcb:CreateWindow
                         :depth 0
                         :wid frame-container
                         :parent exwm--root
                         :x x
                         :y (- y exwm-workspace--window-y-offset)
                         :width width
                         :height height
                         :border-width
                         (with-current-buffer (exwm--id->buffer id)
                           (let ((border-witdh (plist-get exwm--configurations
                                                          'border-width)))
                             (if (and (integerp border-witdh)
                                      (>= border-witdh 0))
                                 border-witdh
                               exwm-floating-border-width)))
                         :class xcb:WindowClass:InputOutput
                         :visual 0
                         :value-mask (logior xcb:CW:BackPixmap
                                             (if border-pixel
                                                 xcb:CW:BorderPixel 0)
                                             xcb:CW:OverrideRedirect)
                         :background-pixmap xcb:BackPixmap:ParentRelative
                         :border-pixel border-pixel
                         :override-redirect 1))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                         :window frame-container
                         :data
                         (format "EXWM floating frame container for 0x%x" id)))
      ;; Map it.
      (xcb:+request exwm--connection
          (make-instance 'xcb:MapWindow :window frame-container))
      ;; Put the X window right above this frame container.
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window id
                         :value-mask (logior xcb:ConfigWindow:Sibling
                                             xcb:ConfigWindow:StackMode)
                         :sibling frame-container
                         :stack-mode xcb:StackMode:Above)))
    ;; Reparent this frame to its container.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ReparentWindow
                       :window outer-id :parent frame-container :x 0 :y 0))
    (exwm-floating--set-allowed-actions id nil)
    (xcb:flush exwm--connection)
    ;; Set window/buffer
    (with-current-buffer (exwm--id->buffer id)
      (setq window-size-fixed exwm--fixed-size
            exwm--floating-frame frame)
      ;; Do the refresh manually.
      (remove-hook 'window-configuration-change-hook #'exwm-layout--refresh)
      (set-window-buffer window (current-buffer)) ;this changes current buffer
      (add-hook 'window-configuration-change-hook #'exwm-layout--refresh)
      (set-window-dedicated-p window t)
      (exwm-layout--show id window))
    (with-current-buffer (exwm--id->buffer id)
      (if (exwm-layout--iconic-state-p id)
          ;; Hide iconic floating X windows.
          (exwm-floating-hide)
        (with-selected-frame exwm--frame
          (exwm-layout--refresh)))
      (select-frame-set-input-focus frame))
    ;; FIXME: Strangely, the Emacs frame can move itself at this point
    ;;        when there are left/top struts set.  Force resetting its
    ;;        position seems working, but it'd better to figure out why.
    ;; FIXME: This also happens in another case (#220) where the cause is
    ;;        still unclear.
    (exwm--set-geometry outer-id 0 0 nil nil)
    (xcb:flush exwm--connection))
  (with-current-buffer (exwm--id->buffer id)
    (run-hooks 'exwm-floating-setup-hook))
  ;; Redraw the frame.
  (redisplay t))

;; Additional commands that should also work in exwm
(exwm-input-set-key (kbd "s-<return>") (lambda () (interactive) (+vterm/toggle nil)))
(exwm-input-set-key (kbd "s-e") (lambda () (interactive) (elfeed-load-summary)))
(exwm-input-set-key (kbd "s-v") (lambda () (interactive) (open-yt-dl-videos)))
(exwm-input-set-key (kbd "s-r") (lambda () (interactive) (progn
  (+vterm/here t)
  (vterm-send-string "cd /home/user/dox/install/rosarium && cargo run\n")
)))
(exwm-input-set-key (kbd "s-<f4>") (lambda () (interactive) (go-to-scratch)))
(exwm-input-set-key (kbd "s-<left>") (lambda () (interactive) (winner-undo)))
(exwm-input-set-key (kbd "s-<right>") (lambda () (interactive) (winner-undo)))
(exwm-input-set-key (kbd "s-a") (lambda () (interactive) (org-agenda-list)))
(exwm-input-set-key (kbd "s-m") (lambda () (interactive) (mu4e--goto-inbox)))
(exwm-input--update-global-prefix-keys)

;; Wallpaper
(setq wallpaper-cycle-directory "/home/user/dox/wallpapers")
(wallpaper-set-wallpaper)

(provide 'exwm-tweaks)
