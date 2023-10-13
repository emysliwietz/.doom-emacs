;;; search.el -*- lexical-binding: t; -*-

;; Swiper / Ivy / Counsel
;;  Swiper gives us a really efficient incremental search with regular expressions
;;  and Ivy / Counsel replace a lot of ido or helms completion functionality

(defun ignore-dired-buffers-ivy (str)
  "Return non-nil if STR names a Dired buffer.
This function is intended for use with `ivy-ignore-buffers'."
  (let ((buf (get-buffer str)))
    (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))

(defun ignore-help-buffers-ivy (str)
  "Return non-nil if STR names a help buffer (buffers starting and ending with *)
This function is intended for use with `ivy-ignore-buffers'."
  (and
   (s-starts-with-p "*" str)
   (s-ends-with-p "*" str)))

(defun ignore-unwanted-buffers-ivy (str)
  "Return non-nil if STR names a Dired buffer.
This function is intended for use with `ivy-ignore-buffers'."
  (or
   (string-equal "elfeed.org" str)
   (member str (map 'list 'file-name-nondirectory org-agenda-files))
   ))

(with-eval-after-load 'ivy
  (progn
  (add-to-list 'ivy-ignore-buffers #'ignore-dired-buffers-ivy)
  (add-to-list 'ivy-ignore-buffers #'ignore-help-buffers-ivy)
  (add-to-list 'ivy-ignore-buffers #'ignore-unwanted-buffers-ivy)
  ))

(use-package! counsel
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

;(use-package! all-the-icons-ibuffer
;  :init (all-the-icons-ibuffer-mode 1))

(defun ivy-icon-switch-buffer ()
  "ivy-switch-buffer with icons"
  (interactive)
  (condition-case nil
      (all-the-icons-ivy-setup))

(defun all-the-icons-ivy--buffer-transformer (b s)
  "Return a candidate string for buffer B named S preceded by an icon.
Try to find the icon for the buffer's B `major-mode'.
If that fails look for an icon for the mode that the `major-mode' is derived from."
  (let ((mode (buffer-local-value 'major-mode b))
	(buffname (replace-regexp-in-string "<.*>$" "" s)))
    (format (concat "%s" all-the-icons-spacer "%s")
            (propertize "\t" 'display (or
                                       (all-the-icons-ivy--icon-for-mode mode)
                                       (all-the-icons-ivy--icon-for-mode (get mode 'derived-mode-parent))
				       (all-the-icons-ivy--icon-for-firefox mode buffname)
				       (all-the-icons-ivy--icon-for-tor mode buffname)
				       (all-the-icons-ivy--icon-for-exwm mode buffname)
                                       (funcall
                                        all-the-icons-ivy-family-fallback-for-buffer
                                        all-the-icons-ivy-name-fallback-for-buffer)))
            (all-the-icons-ivy--buffer-propertize b s))))


  (ivy-switch-buffer))

(setq all-the-icons-ivy-file-commands '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))

;; Overwrite some stuff for exwm and icons in Firefox
(defun all-the-icons-ivy--icon-for-firefox (mode buffname)
  "Apply `all-the-icons-icon-for-url' on Firefox window in exwm-mode.
Assuming that url is in title like in Keepass Helper extension."
  (if (string-equal (format "%s" mode) "exwm-mode")
      (let ((bnl (split-string buffname " - "))
	    (fnl (split-string buffname " — ")))
	    (let ((browser (format "%s" (last fnl))))
      (if (or (string-equal browser "(Mozilla Firefox)") (string-equal browser "(Mozilla Firefox (Private Browsing))"))
	  (all-the-icons-faicon "firefox" :face 'all-the-icons-red)
	)))))

;; Overwrite some stuff for exwm and icons in Tor Browser
(defun all-the-icons-ivy--icon-for-tor (mode buffname)
  "Apply youtube icon on Tor Browser window in exwm-mode.
Not assuming that url is in title like in Keepass Helper extension, for privacy."
  (if (string-equal (format "%s" mode) "exwm-mode")
      (let ((bnl (split-string buffname " - ")))
	(if (string-equal (format "%s" (last bnl)) "(Tor Browser)")
	    (if (string-equal (format "%s" (last bnl 2)) "(YouTube Tor Browser)")
		(all-the-icons-icon-for-url "youtube.com" :face 'all-the-icons-red)
	      (all-the-icons-faicon "user-secret" :face 'all-the-icons-red)
	      )))))

;; Overwrite some stuff for exwm
(defun all-the-icons-ivy--icon-for-exwm (mode buffname)
  "Hard-code some icons for common programs."
  (if (string-equal (format "%s" mode) "exwm-mode")
      (cond ((string-prefix-p "Signal" buffname)
	     (all-the-icons-faicon "comment" :face 'all-the-icons-blue-alt))
	    ((string-prefix-p "Skype" buffname)
	     (all-the-icons-faicon "skype" :face 'all-the-icons-blue))
	    ((string-suffix-p " - Discord" buffname)
	     (all-the-icons-faicon "simplybuilt" :face 'all-the-icons-purple))
	    ((string-prefix-p "OBS" buffname)
	     (all-the-icons-faicon "video-camera" :face 'all-the-icons-purple-alt))
	    ((string-equal "Volume Control" buffname)
	     (all-the-icons-faicon "volume-up" :face 'all-the-icons-purple-alt))
	    ((file-directory-p buffname)
	     (all-the-icons-faicon "folder-open" :face 'all-the-icons-yellow))
	    ((string-suffix-p " - mpv" buffname)
	     (all-the-icons-faicon "play" :face 'all-the-icons-orange))
            ((string-match-p " - Thunar" buffname)
	     (all-the-icons-faicon "folder-open-o" :face 'all-the-icons-blue))
	    ((string-suffix-p "\.java" buffname)
	     (all-the-icons-alltheicon "java" :face 'all-the-icons-orange))
            ((string-suffix-p "\.pdf" buffname)
	     (all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-red))
	    ((or(string-equal "st" buffname) (string-prefix-p (concat (user-login-name) "@") buffname) (string-prefix-p "root@" buffname))
	     (all-the-icons-faicon "terminal" :face 'all-the-icons-green))
	    )))


(provide 'search-ivy)
