;;; auto-correct.el -*- lexical-binding: t; -*-

(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(setq ispell-abbrev-local nil)
(defun ispell-abbrev-advice (ispell-func &rest args)
  "Advice for `ispell-word'. Creates an abbrev for the correction made.
Unless ispell-abbrev-local, abbrev will be global.
ISPELL-FUNC passed as adviced function."
  (let ((before (downcase (or (thing-at-point 'word) "")))
        after
        (res (apply ispell-func args)))

    (setq after (downcase (or (thing-at-point 'word) "")))
    (unless (string= after before)
      (define-abbrev
        (if ispell-abbrev-local local-abbrev-table global-abbrev-table) before after))
    (message "\"%s\" now expands to \"%s\" %sally."
               before after (if ispell-abbrev-local "loc" "glob"))
    res))

(advice-add 'ispell-word :around #'ispell-abbrev-advice)

(provide 'auto-correct)
