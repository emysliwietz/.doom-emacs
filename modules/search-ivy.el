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
(all-the-icons-completion-mode -1)

(cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql buffer)))
  "Return the icon for the candidate CAND of completion category buffer."
  (let* ((mode (buffer-local-value 'major-mode (get-buffer cand)))
         (buffname (buffer-name (get-buffer cand)))
         (icon (nerd-icons-icon-for-mode mode))
         (parent-icon (nerd-icons-icon-for-mode
                       (get mode 'derived-mode-parent))))
    (concat
             (or
              (nerd-icons--icon-for-firefox mode buffname)
	      (all-the-icons-ivy--icon-for-tor mode buffname)
	      (all-the-icons-ivy--icon-for-exwm mode buffname)
              icon
              parent-icon
             (nerd-icons-faicon "nf-fa-sticky_note_o"))
     " "))
   )

;; Overwrite some stuff for exwm and icons in Firefox
(defun nerd-icons--icon-for-firefox (mode buffname)
  "Get icon for Firefox window in exwm-mode.
Assuming that url is in title like in Keepass Helper extension."
  (if (string-equal (format "%s" mode) "exwm-mode")
      (let ((bnl (split-string buffname " - "))
	    (fnl (split-string buffname " — ")))
	    (let ((browser (format "%s" (last fnl))))
      (if (or (string-equal browser "(Mozilla Firefox)") (string-equal browser "(Mozilla Firefox (Private Browsing))"))
          (propertize (nerd-icons-faicon "nf-fa-firefox") 'face '(:foreground "red"))
	)))))

;; Overwrite some stuff for exwm and icons in Tor Browser
(defun all-the-icons-ivy--icon-for-tor (mode buffname)
  "Apply youtube icon on Tor Browser window in exwm-mode.
Not assuming that url is in title like in Keepass Helper extension, for privacy."
  (if (string-equal (format "%s" mode) "exwm-mode")
      (let ((bnl (split-string buffname " - ")))
	(if (string-equal (format "%s" (last bnl)) "(Tor Browser)")
	    (if (string-equal (format "%s" (last bnl 2)) "(YouTube Tor Browser)")
		(nerd-icons-icon-for-url "youtube.com")
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
             (propertize (nerd-icons-faicon "nf-fa-play") 'face '(:foreground "orange")))
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
