;; Email Accounts
(require 'email-refile)
(require 'smtpmail)
(setq mu4e-contexts
      `(
       ,(make-mu4e-context
	  :name "gmail"
	  :enter-func (lambda () (mu4e-message "Switching to gmail context"))
	  :leave-func (lambda () (mu4e-message "Leaving gmail context"))
	  :vars '(
		  ( user-full-name	         . "Egidius Mysliwietz" )
		  ( user-mail-address	         . "e.p.mysliwietz@gmail.com" )
                  ( smtpmail-mail-address        . "e.p.mysliwietz@gmail.com")
		  ( smtpmail-smtp-user           . "e.p.mysliwietz@gmail.com")
		  ( mu4e-drafts-folder           . "/gmail/[Gmail]/Entwürfe" )
                  ( mu4e-sent-folder             . "/gmail/[Gmail]/Gesendet" )
		  ( mu4e-trash-folder            . "/gmail/[Gmail]/Papierkorb" )
		  ( mu4e-refile-folder           . gmail-refile )
                  ( smtpmail-default-smtp-server . "smtp.gmail.com" )
                  ( smtpmail-smtp-server         . "smtp.gmail.com" )
		  ( smtpmail-local-domain        . "gmail.com" )
                  ( smtpmail-smtp-service        . 587 )
		  ))
        ,(make-mu4e-context
	  :name "egidius"
	  :enter-func (lambda () (mu4e-message "Switching to egidius context"))
          :leave-func (lambda () (mu4e-message "Leaving egidius context"))
	  ;; we match based on the contact-fields of the message
	  :vars '(
		  ( user-full-name               . "Egidius Mysliwietz" )
		  ( user-mail-address	         . "egidius@mysliwietz.de"  )
                  ( smtpmail-mail-address        . "egidius@mysliwietz.de")
		  ( smtpmail-smtp-user           . "egidius@mysliwietz.de")
		  ( mu4e-archive-folder          . "/egidius/Archive" )
		  ( mu4e-drafts-folder           . "/egidius/Drafts" )
                  ( mu4e-sent-folder             . "/egidius/Sent" )
		  ( mu4e-trash-folder            . "/egidius/Trash" )
		  ( mu4e-refile-folder           . gmail-refile )
                  ( smtpmail-default-smtp-server . "smtp.strato.de" )
                  ( smtpmail-smtp-server         . "smtp.strato.de" )
		  ( smtpmail-local-domain        . "strato.de" )
                  ( smtpmail-smtp-service        . 465 )
		  ))
      ,(make-mu4e-context
	  :name "xgmx"
	  :enter-func (lambda () (mu4e-message "Switching to gmx context"))
	  :leave-func (lambda () (mu4e-message "Leaving gmx context"))
	  :vars '(
		  ( user-full-name	         . "Egidius Mysliwietz" )
		  ( user-mail-address	         . "egidius.mysliwietz@gmx.de" )
                  ( smtpmail-mail-address        . "egidius.mysliwietz@gmx.de")
		  ( smtpmail-smtp-user           . "egidius.mysliwietz@gmx.de")
		  ( mu4e-archive-folder          . "/gmx/Archiv" )
		  ( mu4e-drafts-folder           . "/gmx/Entwürfe" )
                  ( mu4e-sent-folder             . "/gmx/Gesendet" )
		  ( mu4e-trash-folder            . "/gmx/Gelöscht" )
                  ( smtpmail-default-smtp-server . "mail.gmx.net" )
                  ( smtpmail-smtp-server         . "mail.gmx.net" )
		  ( smtpmail-local-domain        . "gmx.net" )
                  ( smtpmail-smtp-service        . 465 )
		  ))
       ,(make-mu4e-context
	  :name "radboud"
	  :enter-func (lambda () (mu4e-message "Switch to radboud context"))
	  :leave-func (lambda () (mu4e-message "Leaving radboud context"))
	  :vars '(
		  ( user-full-name	         . "Egidius Mysliwietz" )
		  ( user-mail-address	         . "e.mysliwietz@student.ru.nl" )
                  ( smtpmail-mail-address        . "e.mysliwietz@student.ru.nl")
		  ( smtpmail-smtp-user           . "s1000796")
		  ( mu4e-drafts-folder           . "/radboud/Drafts" )
                  ( mu4e-sent-folder             . "/radboud/Sent Items" )
		  ( mu4e-trash-folder            . "/radboud/Trash" )
                  ( smtpmail-default-smtp-server . "smtp-auth.ru.nl" )
                  ( smtpmail-smtp-server         . "smtp-auth.ru.nl" )
		  ( smtpmail-local-domain        . "ru.nl" )
                  ( smtpmail-smtp-service        . 587 )
		  ))
       ,(make-mu4e-context
	  :name "eindhoven"
	  :enter-func (lambda () (mu4e-message "Switch to eindhoven context"))
	  :leave-func (lambda () (mu4e-message "Leaving eindhoven context"))
	  :vars '(
		  ( user-full-name	         . "Egidius Mysliwietz" )
		  ( user-mail-address	         . "e.p.j.mysliwietz@student.tue.nl" )
                  ( smtpmail-mail-address        . "e.p.j.mysliwietz@student.tue.nl")
		  ( smtpmail-smtp-user           . "e.p.j.mysliwietz@student.tue.nl")
                  ( mu4e-archive-folder          . "/eindhoven/Archive" )
		  ( mu4e-drafts-folder           . "/eindhoven/Drafts" )
                  ( mu4e-sent-folder             . "/eindhoven/Sent Items" )
		  ( mu4e-trash-folder            . "/eindhoven/Trash" )
                  ( smtpmail-default-smtp-server . "smtp.office365.com" )
                  ( smtpmail-smtp-server         . "smtp.office365.com" )
		  ( smtpmail-local-domain        . "office365.com" )
                  ( smtpmail-smtp-service        . 587 )
		  ))
       ,(make-mu4e-context
	  :name "ntu"
	  :enter-func (lambda () (mu4e-message "Switch to ntu context"))
	  :leave-func (lambda () (mu4e-message "Leaving ntu context"))
	  :vars '(
		  ( user-full-name	         . "Egidius Mysliwietz" )
		  ( user-mail-address	         . "n1903483e@e.ntu.edu.sg" )
                  ( smtpmail-mail-address        . "n1903483e@e.ntu.edu.sg")
		  ( smtpmail-smtp-user           . "n1903483e@e.ntu.edu.sg")
                  ( mu4e-archive-folder          . "/ntu/Archive" )
		  ( mu4e-drafts-folder           . "/ntu/Drafts" )
                  ( mu4e-sent-folder             . "/ntu/Sent Items" )
		  ( mu4e-trash-folder            . "/ntu/Trash" )
                  ( smtpmail-default-smtp-server . "smtp.office365.com" )
                  ( smtpmail-smtp-server         . "smtp.office365.com" )
		  ( smtpmail-local-domain        . "office365.com" )
                  ( smtpmail-smtp-service        . 587 )
		  ))
       ))


(provide 'email-accounts)
