(defun gmail-refile (msg)
  (let ((subject (mu4e-message-field msg :subject))
	(from-name (car (car (mu4e-message-field msg :from))))
	(from-address (cdr (car (mu4e-message-field msg :from))))
	(to-name (car (car (mu4e-message-field msg :to))))
	(to-address (cdr (car (mu4e-message-field msg :to))))
	(size (mu4e-message-field msg :size))
	(id (mu4e-message-field msg :message-id))
	(path (mu4e-message-field msg :path))
	(priority (mu4e-message-field msg :priority))
	(date (mu4e-message-field msg :date))
	(flags (mu4e-message-field msg :flags))
	(maildir (mu4e-message-field msg :maildir))
	)
    (write-region (format
		   "Subject: %s
From-name: %s
From-address: %s
To-name: %s
To-address: %s
Size: %s
ID: %s
Path: %s
Priority: %s
Date: %s
Flags: %s
Maildir: %s

Message: %s" subject from-name from-address to-name to-address size id path priority date flags maildir msg) nil "/tmp/message")
    (shell-command-to-string (concat "/usr/bin/python3 /home/user/.config/emacs/modules/refile.py"))))
  ;(cond
   ;((string-prefix-p
     ;"Activity summary for 1920 Schrijven Over Wetenschap (KW4 V)"
     ;(mu4e-message-field msg :subject))
    ;"/gmail/RU/RU Mails/Q12/SoW")
   ;((string-prefix-p
     ;"Activity summary for 2021 Hardware Security (KW3 V)"
     ;(mu4e-message-field msg :subject))
    ;"/gmail/RU/RU Mails/M03/Hardware_Security")
   ;((string-prefix-p
     ;"Activity summary for 2021 Security in Organisations (KW1 V)"
     ;(mu4e-message-field msg :subject))
    ;"/gmail/RU/RU Mails/M01/Security in Organizations")
   ;
   ;((string-match-p
     ;"mommajux@yahoo\.com"
     ;(mu4e-message-field msg :subject))
    ;"/gmail/Bekannte/Eisenlohr, Alexandra")
   ;
   ;((string-match-p
     ;"@.*gmx\.net"
     ;(first (quote(mu4e-message-field msg :from))))
    ;"/gmail/gmx")

      ;; messages sent by me go to the sent folder
;      ((find-if
;	 (lambda (addr)
;	   (mu4e-message-contact-field-matches msg :from addr))
;	 (mu4e-personal-addresses))
;	mu4e-sent-folder)
      ;; everything else goes to /archive
      ;; important to have a catch-all at the end!
					;     (t  "/unsorted"))



(provide 'email-refile)
