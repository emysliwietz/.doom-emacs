(require 'async)

(async-start
 (lambda ()
   (require 'secrets)
   (let ((cal1 (secrets-get-secret "Passwords" "Org GCal Calendar 1"))
         (cal2 (secrets-get-secret "Passwords" "Org GCal Calendar 2")))
     `(
       (,cal1 .  "~/sync/agenda/gcal.org")
       (,cal2 .  "~/sync/agenda/gcal.org")
       )))
 (lambda (result)
   (setq org-gcal-fetch-file-alist result)))

(setq org-gcal-notify-p t
      plstore-cache-passphrase-for-symmetric-encryption t)


(async-start
 (lambda ()
   (require 'secrets)
   `(,(secrets-get-secret "Passwords" "Org GCal Client ID")
     ,(secrets-get-secret "Passwords" "Org GCal Client Secret"))
   )
 (lambda (result)
   (let ((id (nth 0 result))
         (sc (nth 1 result))
         )
     (setq org-gcal-client-id id
           org-gcal-client-secret sc)
     (org-gcal-reload-client-id-secret)
     (require 'oauth2-auto)
     (require 'plstore)
     (require 'org-gcal)
     )))

(provide 'org-g-cal-tweaks)
