;;; email-config.el -*- lexical-binding: t; -*-

(require 'mu4e)
(require 'smtpmail)
(define-key mu4e-view-mode-map (kbd "f") 'mu4e-view-go-to-url)

(setq mu4e-root-maildir "~/mail"
      ;mu4e-get-mail-command "offlineimap -q -f INBOX"
      mu4e-get-mail-command "mbsync -a || true"
      mu4e-update-interval 300 ;; second
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-prefer-html t
      mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain"
      mu4e-headers-auto-update t
      mu4e-compose-format-flowed t
      sendmail-program "/usr/bin/msmtp"
      smtpmail-stream-type 'starttls
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
;      message-send-mail-function 'smtpmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      mu4e-view-show-addresses t
      mu4e-split-view 'single-window ;; horizontal (default), vertical
      mu4e-attachment-dir "~/Downloads"
      smtpmail-queue-mail nil
      smtpmail-queue-dir "~/mail/queue/cur"
      mu4e-compose-in-new-frame nil
      mu4e-compose-dont-reply-to-self t
      mu4e-headers-date-format "%Y-%m-%d %H:%M"
      message-kill-buffer-on-exit nil
      mu4e-confirm-quit nil
      mu4e-context-policy 'ask-if-none
      mu4e-compose-context-policy 'always-ask
      mu4e-headers-results-limit 500
      mu4e-use-fancy-chars t)

(defun mu4e--view-quit-and-back ()
  "Quit mu4e view buffer and go back to mu4e"
  (interactive)
  (mu4e-view-quit)
  (mu4e--goto-inbox))

(defun mu4e--goto-inbox ()
  "Goto mu4e inbox"
  (interactive)
  (mu4e~headers-jump-to-maildir "/gmail/INBOX"))

(map! :map mu4e-view-mode-map
      :after mu4e-view
      :n "<backspace>" 'mu4e--view-quit-and-back)

(global-set-key (kbd "s-m") 'mu4e--goto-inbox)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

; Make sure doom doesn't move mu4e article buffer into popup
; Show it in same window
(set-popup-rule! "*mu4e-article*" :ignore 1)

;(require 'org-mu4e)
;(setq org-mu4e-convert-to-html t
;org-mu4e-link-query-in-headers-mode nil)

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

; Ignore popup rules to make sure emails are shown in same window
(after! mu4e
  (set-popup-rule! "^\\*mu4e-headers\\*" :ignore t)
  (set-popup-rule! "^\\*mu4e-view\\*" :ignore t))


(provide 'email-config)
