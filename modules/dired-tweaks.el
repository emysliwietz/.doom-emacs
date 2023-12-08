;;; dired-tweaks.el -*- lexical-binding: t; -*-

;; Dired
;;; Colourful dired
(use-package! diredfl
  :init (diredfl-global-mode 1))

(use-package! all-the-icons-dired
  :config
  ;(all-the-icons-dired-mode 1)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)))


(define-minor-mode dired-follow-mode
  "Diplay file at point in dired after a move."
  :lighter " dired-f"
  :global t
  (if dired-follow-mode
      (advice-add 'dired-next-line :after (lambda (arg) (dired-display-file)))
    (advice-remove 'dired-next-line (lambda (arg) (dired-display-file)))))

(setq vc-follow-symlinks t
      dired-listing-switches "-ahlt"
      diredp-toggle-find-file-reuse-dir 1
      image-dired-thumb-size 100
      diredp-image-preview-in-tooltip 100
      dired-auto-revert-buffer t
      diredp-hide-details-initially-flag nil
      dired-hide-details-mode 0)

(defmacro image-view (direction)
  `(lambda ()
     (interactive)
     (quit-window)
     (let ((pt (point))
           filename)
       (or (ignore-errors
             (catch 'filename
               (while (dired-next-line ,direction)
                 (when (image-type-from-file-name
                        (setq filename (dired-get-filename)))
                   (throw 'filename filename)))))
           (goto-char pt))
       (dired-view-file))))

(eval-after-load "image-mode"
  '(progn
    (define-key image-mode-map "n" (image-view 1))
    (define-key image-mode-map "p" (image-view -1))))

;(use-package dired-k
;  ;; use dired-k as alternative to revert buffer. This will refresh git status
;  :hook (dired-mode . dired-k)
;  :bind (:map dired-mode-map
;              ("g" . dired-k)))

(use-package! diredful
  :config (diredful-mode 1))

(use-package! dired-git-info
  :config
  (setq dgi-auto-hide-details-p nil)
  (add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable))


(use-package! async
  :init (dired-async-mode 1))

(use-package! dired-quick-sort
  :config
  (dired-quick-sort-setup)
  (setq dired-quick-sort-suppress-setup-warning t))

(use-package! openwith
  :config
  (setq openwith-associations
        (cond
         ((string-equal system-type "darwin")
          '(("\\.\\(dmg\\|doc\\|docs\\|xls\\|xlsx\\)$"
             "open" (file))
            ("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
             "open" ("-a" "VLC" file))))
         ((string-equal system-type "gnu/linux")
          '(("\\.\\(mp4\\|m4a\\|mp3\\|mkv\\|webm\\|avi\\|flv\\|mov\\|part\\)$" ; removed \\|pdf
             "xdg-open" (file))))))
  (openwith-mode t)
  (setq large-file-warning-threshold 3000000000))

(define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)

;; Docview j and k go forward a line which is weird behaviour in a pdf
;; Paging is prefered to scrolling
(after! doc-view-mode
(define-key doc-view-mode-map (kbd "j") 'doc-view-next-page)
(define-key doc-view-mode-map (kbd "k") 'doc-view-previous-page))

;; Convert files automatically
(defun dired-convert-file ()
  "Converts pptx or docx files to pdf"
  (interactive)
  (cl-map 'nil '(lambda (file)
                   (let ((ext (file-name-extension file))
                         (base-name-sans-ext (file-name-sans-extension (file-name-nondirectory file))))
                     (cond
                      ((or (string-equal ext "pptx") (string-equal ext "ppt"))
                       (async-shell-command (format "libreoffice --headless --invisible --convert-to pdf \"%s\"" file)))
                      ((or (string-equal ext "docx") (string-equal ext "doc") (string-equal ext "epub") (string-equal ext "tex") (string-equal "html") (string-equal ext "org") (string-equal ext "txt"))
                       (async-shell-command (format "pandoc -i \"%s\" -o \"%s.pdf\"" file base-name-sans-ext)))
                      ((or (string-equal ext "jpg") (string-equal ext "jpeg") (string-equal ext "png"))
                       (async-shell-command (format "convert \"%s\" -rotate 90 \"%s\"" file file)))
                      ))) (dired-get-marked-files)))

;; Rotate pdf file
(defun dired-rotate-pdf ()
  "Rotate a pdf file in-place"
  (interactive)
  (cl-map 'nil '(lambda (file)
                   (let ((ext (file-name-extension file))
                         (base-name-sans-ext (file-name-sans-extension (file-name-nondirectory file))))
                     (when (string-equal "pdf" ext )
                       (message file)
                       (async-shell-command-no-window
                        (format "mutool draw -R90 -o \"%s\" \"%s\"" file file))
                       )
                )) (dired-get-marked-files)))


;; Vertically split pdf file
(defun dired-split-pdf-vertical ()
  "Vertically split a pdf file in-place"
  (interactive)
  (cl-map 'nil '(lambda (file)
                   (let ((ext (file-name-extension file))
                         (base-name-sans-ext (file-name-sans-extension (file-name-nondirectory file))))
                     (when (string-equal "pdf" ext )
                       (message file)
                       (async-shell-command-no-window
                        (format "mutool poster -x2 \"%s\" \"%s\"" file file))
                       )
                )) (dired-get-marked-files)))

;; Horizontally split pdf file
(defun dired-split-pdf-horizontal ()
  "Horizontally split a pdf file in-place"
  (interactive)
  (cl-map 'nil '(lambda (file)
                   (let ((ext (file-name-extension file))
                         (base-name-sans-ext (file-name-sans-extension (file-name-nondirectory file))))
                     (when (string-equal "pdf" ext )
                       (message file)
                       (async-shell-command-no-window
                        (format "mutool poster -y2 \"%s\" \"%s\"" file file))
                       )
                )) (dired-get-marked-files)))




;; Toggle youtube-dl-list if in elfeed-youtube buffer, else perform regular load
(defun dired-load-or-youtube-toggle ()
  (interactive)
  (cond ((string-equal (buffer-name) "elfeed-youtube")
         (youtube-dl-list))
        ((eq major-mode 'youtube-dl-list-mode) (kill-buffer))
        (t (dired-do-load))))

(map! :map dired-mode-map
      ;:after dired-mode
      ;:n doom-leader-key nil
      :n "c" #'dired-convert-file
      :n "L" #'dired-load-or-youtube-toggle)

(map! :map youtube-dl-list-mode-map
      :n "L" #'dired-load-or-youtube-toggle
      :n "r" #'youtube-dl-failures-reset)

; Allow dragging files from dired to other applications
(setq dired-mouse-drag-files t)

(provide 'dired-tweaks)
