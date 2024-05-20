;; deutsche Feiertage
;; Show holidays in Org Agenda
(setq org-agenda-include-diary t)

(setq calendar-week-start-day 1
      calendar-time-display-form '(24-hours ":" minutes)
      calendar-date-style 'european
      calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                               "Donnerstag" "Freitag" "Samstag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                 "Juni" "Juli" "August" "September"
                                 "Oktober" "November" "Dezember"])

(setq solar-n-hemi-seasons
      '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang"))


(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")))

(setq holiday-christian-holidays
      '((holiday-float 12 0 -4 "1. Advent" 24)
        (holiday-float 12 0 -3 "2. Advent" 24)
        (holiday-float 12 0 -2 "3. Advent" 24)
        (holiday-float 12 0 -1 "4. Advent" 24)
        (holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
        (holiday-fixed 1 6 "Heilige Drei Könige")
        (holiday-easter-etc -48 "Rosenmontag")
        (holiday-easter-etc -3 "Gründonnerstag")
        (holiday-easter-etc  -2 "Karfreitag")
        (holiday-easter-etc   0 "Ostersonntag")
        (holiday-easter-etc  +1 "Ostermontag")
        (holiday-easter-etc +39 "Christi Himmelfahrt")
        (holiday-easter-etc +49 "Pfingstsonntag")
        (holiday-easter-etc +50 "Pfingstmontag")
        (holiday-easter-etc +60 "Fronleichnam")
        (holiday-fixed 8 15 "Mariae Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")
        (holiday-float 11 3 1 "Buss- und Bettag" 16)
        (holiday-float 11 0 1 "Totensonntag" 20)))

(setq calendar-holidays '(
      (solar-equinoxes-solstices)
      (holiday-sexp calendar-daylight-savings-starts
                    (format "Daylight Saving Time Begins %s"
                            (solar-time-string
                             (/ calendar-daylight-savings-starts-time
                                (float 60))
                             calendar-standard-time-zone-name)))
      (holiday-sexp calendar-daylight-savings-ends
                    (format "Daylight Saving Time Ends %s"
                            (solar-time-string
                             (/ calendar-daylight-savings-ends-time
                                (float 60))
                             calendar-daylight-time-zone-name)))
     (holiday-sexp
       '(if (zerop (% year 4))
           (calendar-gregorian-from-absolute
             (1+ (calendar-dayname-on-or-before
                   1 (+ 6 (calendar-absolute-from-gregorian
                            (list 11 1 year)))))))
       "US Presidential Election")
;(lunar-phases)
))

(add-to-list 'calendar-holidays holiday-general-holidays)
(add-to-list 'calendar-holidays holiday-christian-holidays)
(add-to-list 'calendar-holidays holiday-hebrew-holidays)
(add-to-list 'calendar-holidays holiday-islamic-holidays)
(add-to-list 'calendar-holidays holiday-oriental-holidays)

(provide 'calendar-tweaks)
