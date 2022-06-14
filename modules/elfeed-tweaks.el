;;; elfeed-tweaks.el -*- lexical-binding: t; -*-

(setq rmh-elfeed-org-files (cons (expand-file-name "ext/elfeed/elfeed.org" doom-private-dir)())
      elfeed-db-directory (expand-file-name "ext/elfeed/db/" doom-private-dir)
      )


(map! :map elfeed-search-mode-map
      :after elfeed-search
                                        ;[remap kill-this-buffer] "q"
                                        ;[remap kill-buffer] "q"
      :n doom-leader-key nil
      :n "q" #'elfeed-save-summary
      :n "e" #'elfeed-update
      :n "r" #'elfeed-search-untag-all-unread
      :n "u" #'elfeed-search-tag-all-unread
      :n "s" #'elfeed-search-live-filter
      :n "RET" #'elfeed-search-show-entry
      :n "p" #'elfeed-show-pdf
      :n "v" #'elfeed-search-youtube-dl
      :n "L" #'youtube-dl-list
      :n "+" #'elfeed-search-tag-all
      :n "-" #'elfeed-search-untag-all
      :n "S" #'elfeed-search-set-filter
      :n "b" #'elfeed-search-browse-url
      :n "y" #'elfeed-search-yank)
(map! :map elfeed-show-mode-map
      :after elfeed-show
                                        ;[remap kill-this-buffer] "q"
                                        ;[remap kill-buffer] "q"
      :n doom-leader-key nil
      :nm "q" #'elfeed-save-close
      :nm "o" #'ace-link-elfeed
      :nm "RET" #'org-ref-elfeed-add
      :nm "n" #'elfeed-show-next
      :nm "N" #'elfeed-show-prev
      :nm "p" #'elfeed-show-pdf
      :nm "v" #'elfeed-show-youtube-dl
      :nm "L" #'youtube-dl-list
      :nm "+" #'elfeed-show-tag
      :nm "-" #'elfeed-show-untag
      :nm "s" #'elfeed-show-new-live-search
      :nm "y" #'elfeed-show-yank)
(map! :map elfeed-summary-mode-map
      :after elfeed-summary
      :n "L" #'youtube-dl-list
      :n "V" #'open-yt-dl-videos
      :n "R" #'elfeed-summary-load-update
      :n "RET" #'elfeed-summary-action-save-location)

(after! elfeed-search
  (set-evil-initial-state! 'elfeed-search-mode 'normal))
(after! elfeed-show-mode
  (set-evil-initial-state! 'elfeed-show-mode   'normal))

(after! evil-snipe
  (push 'elfeed-show-mode   evil-snipe-disabled-modes)
  (push 'elfeed-search-mode evil-snipe-disabled-modes))

(after! elfeed
  (elfeed-org)
  (use-package! elfeed-link)
  (elfeed-db-load)
  (setq elfeed-search-filter "@1-week-ago +unread"
        elfeed-summary--only-unread t
        elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
        shr-max-image-proportion 0.6)

  (add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
  (add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

  (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-author-face `((t (:weight light)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground 'nil
                      :weight 'light)

  (defadvice! +rss-elfeed-wrap-h-nicer ()
    "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column' and centering it with `visual-fill-column-mode'."
    :override #'+rss-elfeed-wrap-h
    (setq-local truncate-lines nil
                shr-width 120
                visual-fill-column-center-text t
                default-text-properties '(line-height 1.1))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (visual-fill-column-mode)
      (setq-local shr-current-font '(:family "Linux Libertine O" :height 1.2))
      (set-buffer-modified-p nil)))

  (defun +rss/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((elfeed-goodies/tag-column-width 40)
           (elfeed-goodies/feed-source-column-width 30)
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat (mapconcat 'identity tags ",")))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))

           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left)))

                                        ;(insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
                                        ;(insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))
      (setq-local line-spacing 0.2)))

  (defun +rss/elfeed-show-refresh--better-style ()
    "Update the buffer to match the selected entry, using a mail-style."
    (interactive)
    (let* ((inhibit-read-only t)
           (title (elfeed-entry-title elfeed-show-entry))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (author (elfeed-meta elfeed-show-entry :author))
           (link (elfeed-entry-link elfeed-show-entry))
           (tags (elfeed-entry-tags elfeed-show-entry))
           (tagsstr (mapconcat #'symbol-name tags ", "))
           (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
           (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
           (type (elfeed-entry-content-type elfeed-show-entry))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (feed-title (elfeed-feed-title feed))
           (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
      (erase-buffer)
      (insert "\n")
      (insert (format "%s\n\n" (propertize title 'face 'elfeed-show-title-face)))
      (insert (format "%s\t" (propertize feed-title 'face 'elfeed-search-feed-face)))
      (when (and author elfeed-show-entry-author)
        (insert (format "%s\n" (propertize author 'face 'elfeed-show-author-face))))
      (insert (format "%s\n\n" (propertize nicedate 'face 'elfeed-log-date-face)))
      (when tags
        (insert (format "%s\n"
                        (propertize tagsstr 'face 'elfeed-search-tag-face))))
      ;; (insert (propertize "Link: " 'face 'message-header-name))
      ;; (elfeed-insert-link link link)
      ;; (insert "\n")
      (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
               do (insert (propertize "Enclosure: " 'face 'message-header-name))
               do (elfeed-insert-link (car enclosure))
               do (insert "\n"))
      (insert "\n")
      (if content
          (if (eq type 'html)
              (elfeed-insert-html content base)
            (insert content))
        (insert (propertize "(empty)\n" 'face 'italic)))
      (goto-char (point-min))))

  )

(after! elfeed-show
  (require 'url)

  (defvar elfeed-pdf-dir
    (expand-file-name "pdfs/"
                      (file-name-directory (directory-file-name elfeed-enclosure-default-dir))))

  (defvar elfeed-link-pdfs
    '(("https://www.jstatsoft.org/index.php/jss/article/view/v0\\([^/]+\\)" . "https://www.jstatsoft.org/index.php/jss/article/view/v0\\1/v\\1.pdf")
      ("http://arxiv.org/abs/\\([^/]+\\)" . "https://arxiv.org/pdf/\\1.pdf"))
    "List of alists of the form (REGEX-FOR-LINK . FORM-FOR-PDF)")

  (defun elfeed-show-pdf (entry)
    (interactive
     (list (or elfeed-show-entry (elfeed-search-selected :ignore-region))))
    (let ((link (elfeed-entry-link entry))
          (feed-name (plist-get (elfeed-feed-meta (elfeed-entry-feed entry)) :title))
          (title (elfeed-entry-title entry))
          (file-view-function
           (lambda (f)
             (when elfeed-show-entry
               (elfeed-kill-buffer))
             (pop-to-buffer (find-file-noselect f))))
          pdf)

      (let ((file (expand-file-name
                   (concat (subst-char-in-string ?/ ?, title) ".pdf")
                   (expand-file-name (subst-char-in-string ?/ ?, feed-name)
                                     elfeed-pdf-dir))))
        (if (file-exists-p file)
            (funcall file-view-function file)
          (dolist (link-pdf elfeed-link-pdfs)
            (when (and (string-match-p (car link-pdf) link)
                       (not pdf))
              (setq pdf (replace-regexp-in-string (car link-pdf) (cdr link-pdf) link))))
          (if (not pdf)
              (message "No associated PDF for entry")
            (message "Fetching %s" pdf)
            (unless (file-exists-p (file-name-directory file))
              (make-directory (file-name-directory file) t))
            (url-copy-file pdf file)
            (funcall file-view-function file))))))

  )

(after! elfeed-summary
  (elfeed-org))

(defun elfeed-save-summary ()
  "Save database and go to summary"
  (interactive)
  (elfeed-db-save-safe)
  (kill-this-buffer)
  (elfeed-summary)
  (when (boundp 'elfeed-summary--current-pos)
    (goto-char elfeed-summary--current-pos)))

(defun elfeed-save-close ()
  "Save database and close rss"
  (interactive)
  (elfeed-db-save-safe)
  (+rss/delete-pane))

(defun elfeed-load-summary ()
  "Load database and go to summary"
  (interactive)
  (when (functionp 'elfeed-db-load)
    (elfeed-db-load))
  (elfeed-summary))

(defun elfeed-summary-load-update ()
  "Loads the database again before updating"
  (interactive)
  (elfeed-db-load)
  (message "Refreshing db...")
  (elfeed-update)
  (elfeed-summary-update))

(setq elfeed-summary-settings
      '(
        (group (:title . "Blogs [Security]")
               (:elements
                (query . (and people security))))
        (group (:title . "Blogs [People]")
               (:elements
                (query . (and people (not security)))
                ))
        (group (:title . "Religion")
               (:elements
                (query . religion)))
        (group (:title . "Cooking")
               (:elements
                (query . cooking)))
        (group (:title . "ASMR")
               (:elements
                (query . asmr)))
        (group (:title . "Crafting")
               (:elements
                (query . crafting)))
        (group (:title . "Entertainment")
               (:elements
                (query . entertainment)))
        (group (:title . "Finances")
               (:elements
                (query . finances)))
        (group (:title . "Foreign Places")
               (:elements
                (query . foreign_places)))
        (group (:title . "Geography")
               (:elements
                (query . geography)))
        (group (:title . "History")
               (:elements
                (query . history)))
        (group (:title . "Language")
               (:elements
                (query . language)))
        (group (:title . "Math")
               (:elements
                (query . music)))
        (group (:title . "Nature")
               (:elements
                (query . nature)))
        (group (:title . "Philosophy")
               (:elements
                (query . philosophy)))
        (group (:title . "Politics")
               (:elements
                (query . politics)))
        (group (:title . "Science")
               (:elements
                (query . science)))
        (group (:title . "SCP")
               (:elements
                (query . scp)))
        (group (:title . "Tech")
               (:elements
                (query . tech)))
        (group (:title . "Podcasts")
               (:elements
                (query . podcast)))
        (group (:title . "Pictures")
               (:elements
                (query . picture)))
        ;; ...
        (group (:title . "Miscellaneous")
               (:elements
                                        ;(group
                                        ; (:title . "Searches")
                                        ; (:elements
                                        ;  (search
                                        ;   (:filter . "@6-months-ago")
                                        ;   (:title . "Unread"))))
                (group
                 (:title . "Ungrouped")
                 (:elements :misc))))))
(global-set-key (kbd "s-e") 'elfeed-load-summary)

                                        ; Elfeed Youtube

                                        ; External youtube-dl library
(add-to-list 'load-path "~/.doom.d/lisp/youtube-dl-emacs")
(after-startup (require 'youtube-dl))
(setq youtube-dl-directory "/tmp/elfeed-youtube"
      youtube-dl-program "yt-dlp"
      youtube-dl-arguments
      (nconc `("-f" "bestvideo[height<=1080]+bestaudio/best[height<=1080]"
               "--sponsorblock-remove" "default"
               "-prefer-free-formats"
               "-embed-subs"
               "-embed-metadata"
               "-embed-chapters"
               ;"-ffmpeg-location" "path"
               "--no-colors")
             youtube-dl-arguments))
                                        ; (setq youtube-dl-arguments nil)

(global-set-key (kbd "s-v") 'open-yt-dl-videos)

(defun open-yt-dl-videos ()
  (interactive)
  (find-file youtube-dl-directory))

(cl-defun elfeed-show-youtube-dl (&key slow)
  "Download the current entry with youtube-dl."
  (interactive)
  (if (null (youtube-dl (elfeed-entry-link elfeed-show-entry)
                        :title (elfeed-entry-title elfeed-show-entry)
                        :slow slow))
      (message "Entry is not a YouTube link!")
    (message "Downloading %s" (elfeed-entry-title elfeed-show-entry))))


(cl-defun elfeed-search-youtube-dl (&key slow)
  "Download the current entry with youtube-dl."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (dolist (entry entries)
      (if (null (youtube-dl (elfeed-entry-link entry)
                            :title (elfeed-entry-title entry)
                            :slow slow))
          (message "Entry is not a YouTube link!")
        (message "Downloading %s" (elfeed-entry-title entry)))
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (unless (use-region-p) (forward-line)))))

(defun youtube-dl-list-url ()
  "Return url of item under point."
  (interactive)
  (let* ((n (1- (line-number-at-pos)))
         (item (nth n youtube-dl-items)))
    (when item
      (message (youtube-dl-item-destination item)))))
                                        ; Faces

(defface elfeed-youtube
  '((t :foreground "purple"))
  "Marks YouTube videos in Elfeed."
  :group 'elfeed)

(defface elfeed-religion
  '((t :foreground "gold"))
  "Marks YouTube videos in Elfeed."
  :group 'elfeed)

(defface elfeed-tech
  '((t :foreground "LightSteelBlue4"))
  "Marks Tech videos in Elfeed."
  :group 'elfeed)

(defun elfeed-summary-action-save-location (pos &optional event)
  (interactive "@d")
  (setq elfeed-summary--current-pos pos)
  (elfeed-summary--action pos event)
  )



(defun image-tooltip (window object position)
  (save-excursion
    (goto-char position)
                                        ;(message "%s" "Hello")
    (propertize "Look in minbuffer"
                'display (create-image (expand-file-name "/tmp/test.jpg")))))



(defun elfeed-search-thumbnail ()
  (interactive)
  (tooltip-mode t)
  (font-lock-add-keywords
   nil
   '(("\([^<]+\)" 0 '(face font-lock-keyword-face
                           help-echo image-tooltip)))))

(push '(youtube elfeed-youtube)
      elfeed-search-face-alist)
(push '(religion elfeed-religion)
      elfeed-search-face-alist)
(push '(tech elfeed-tech)
      elfeed-search-face-alist)

(add-hook! 'elfeed-search-mode-hook 'elfeed-search-thumbnail)


(provide 'elfeed-tweaks)
