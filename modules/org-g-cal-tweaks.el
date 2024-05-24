(setq org-gcal-client-id (secrets-get-secret "Passwords" "Org GCal Client ID")
      org-gcal-client-secret (secrets-get-secret "Passwords" "Org GCal Client Secret")
      org-gcal-fetch-file-alist `(
                                  (,(secrets-get-secret "Passwords" "Org GCal Calendar 1") .  "~/sync/agenda/gcal.org")
                                  (,(secrets-get-secret "Passwords" "Org GCal Calendar 2") .  "~/sync/agenda/gcal.org")
                                  )
      org-gcal-notify-p t
      plstore-cache-passphrase-for-symmetric-encryption t)


(require 'oauth2-auto)
(require 'plstore)
(require 'org-gcal)

(provide 'org-g-cal-tweaks)
