;;; email-config.el -*- lexical-binding: t; -*-

(require 'mu4e)
(require 'smtpmail)

(setq
message-send-mail-function 'smtpmail-send-it
starttls-use-gnutls t
mu4e-sent-messages-behavior 'sent)

(define-key mu4e-view-mode-map (kbd "f") 'mu4e-view-go-to-url)

(setq mu4e-root-maildir "~/mail"
mu4e-get-mail-command "offlineimap -q -f INBOX"
mu4e-update-interval 60 ;; second
mu4e-compose-signature-auto-include nil
mu4e-view-show-images t
mu4e-view-prefer-html nil
;      mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain"
mu4e-headers-auto-update t
mu4e-compose-format-flowed t
smtpmail-stream-type 'starttls
mu4e-view-show-addresses t
mu4e-split-view 'single-window ;; horizontal (default), vertical
mu4e-attachment-dir "~/Downloads"
smtpmail-queue-mail nil
smtpmail-queue-dir "~/mail/queue/cur"
mu4e-compose-in-new-frame t
mu4e-compose-dont-reply-to-self t
mu4e-headers-date-format "%Y-%m-%d %H:%M"
message-kill-buffer-on-exit t
mu4e-confirm-quit nil
mu4e-headers-results-limit 500
mu4e-use-fancy-chars t)

(when (fboundp 'imagemagick-register-types)
(imagemagick-register-types))

(require 'org-mu4e)
(setq org-mu4e-convert-to-html t
org-mu4e-link-query-in-headers-mode nil)

(require 'org-mime)

;; this seems to fix the babel file saving thing
(defun org~mu4e-mime-replace-images (str current-file)
"Replace images in html files with cid links."
(let (html-images)
(cons
(replace-regexp-in-string ;; replace images in html
"src=\"\\([^\"]+\\)\""
(lambda (text)
(format
        "src=\"./:%s\""
        (let* ((url (and (string-match "src=\"\\([^\"]+\\)\"" text)
                        (match-string 1 text)))
        (path (expand-file-name
                url (file-name-directory current-file)))
        (ext (file-name-extension path))
        (id (replace-regexp-in-string "[\/\\\\]" "_" path)))
        (add-to-list 'html-images
                (org~mu4e-mime-file
                (concat "image/" ext) path id))
        id)))
str)
html-images)))

(add-to-list 'mu4e-view-actions
'("ViewInBrowser" . mu4e-action-view-in-browser) t)


(provide 'email-config)
