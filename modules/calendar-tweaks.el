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


(setq holiday-german-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")))

(setq holiday-german-christian-holidays
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
        (holiday-fixed 8 15 "Mariæ Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")
        (holiday-fixed 11 2 "Allerseelen")
        (holiday-float 11 3 1 "Buß- und Bettag" 16)
        (holiday-float 11 0 1 "Totensonntag" 20)))

(setq holiday-filipino-general-holidays
      '((holiday-fixed 1 23 "1st Philippine Republic Day")
        (holiday-fixed 4 9 "The Day of Valor (PH)")
        (holiday-fixed 6 12 "Filipino Independence Day")
        (holiday-fixed 8 21 "Ninoy Aquino Day")
        (holiday-fixed 9 3 "Yamashita Surrender (PH)")
        (holiday-fixed 12 30 "Jose Rizal Day")))

(setq holiday-filipino-christian-holidays
      '((holiday-fixed 9 8 "Feast of the Nativity of Mary")
        (holiday-fixed 11 30 "Bonifacio Day")))

(setq holiday-us-elections
      '((holiday-sexp
       '(if (zerop (% year 4))
           (calendar-gregorian-from-absolute
             (1+ (calendar-dayname-on-or-before
                   1 (+ 6 (calendar-absolute-from-gregorian
                            (list 11 1 year)))))))
       "US Presidential Election")
))

(setq calendar-holidays
      (append holiday-general-holidays holiday-local-holidays holiday-other-holidays
	      holiday-solar-holidays holiday-christian-holidays holiday-german-general-holidays holiday-german-christian-holidays holiday-us-elections holiday-hebrew-holidays holiday-islamic-holidays holiday-oriental-holidays holiday-filipino-general-holidays holiday-filipino-christian-holidays))

(global-set-key (kbd "s-c") '=calendar)

(provide 'calendar-tweaks)
