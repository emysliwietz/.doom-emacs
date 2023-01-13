;;; bible.el -*- lexical-binding: t; -*-

(defun bible-books-long (binary)
  "Return list of books of the Bible"
  (split-string-and-unquote (shell-command-to-string (format "%s -l | awk 'NF{NF--}; 1' | sed '1s/^\xEF\xBB\xBF//'" binary)) "\n"))

(defun bible-books-short (binary)
  "Return list of abbreviated books of the Bible"
  (split-string-and-unquote (shell-command-to-string (format "%s -l | awk '{print $(NF)}' | sed 's/^\(//;s/\)$//;1s/^\xEF\xBB\xBF//'" binary)) "\n"))

(defun bible-text (binary book-chapter-and-verse)
  "Return text of Bible binary at specified book, chapter and verse"
  (let* ((bcv1 (string-replace "." "" book-chapter-and-verse))
        (bcv (string-replace "," ":" bcv1)))
  (shell-command-to-string (format "%s %s | sed 's/\r//g'" binary bcv))))

(setq bible-minor-mode--last-book ""
      bible-minor-mode--last-chapter-and-verse "")

(defun bible-select (binary)
  "Ask user to select bible verse using kjv-like binary"
  (let* ((book (ivy-read "Select Book: " (bible-books-long binary) :preselect bible-minor-mode--last-book))
         (chapter-and-verse (ivy-read (format "Choose Chapter and Verse of %s: " book) '() :initial-input bible-minor-mode--last-chapter-and-verse)))
    (setq bible-minor-mode--last-book book
          bible-minor-mode--last-chapter-and-verse chapter-and-verse)
    (format "%s %s" book chapter-and-verse)))

(defun bible-insert (&optional verse)
  "Inserts text of the Bible of selected verse"
  (interactive)
  (if verse
      (insert (bible-text (bible-minor-mode--default-bible-binary) verse))
    (insert (bible-text (bible-minor-mode--default-bible-binary) (bible-select)))))

(defun bible-buffer-update (verse)
  "Updates the Bible buffer with verse"
  (switch-to-buffer "*Bible*")
  (erase-buffer)
  (bible-insert verse)
  (bible-mode))

(defun bible ()
  "Open the Bible at selected verse"
  (interactive)
  (let ((bible-verse (bible-select (bible-minor-mode--default-bible-binary))))
    (bible-buffer-update bible-verse)
    (goto-char 0)))

(defun kjv ()
  "Insert Bible Verse from the King James Version"
  (interactive)
  (let ((bin "kjv"))
    (insert (bible-text bin (bible-select bin)))))

(defun menge ()
  "Insert Bible Verse from the Menge Bible"
  (interactive)
  (let ((bin "menge"))
    (insert (bible-text bin (bible-select bin)))))

(defun vul ()
  "Insert Bible Verse from the Vulgate"
  (interactive)
  (let ((bin "vul"))
    (insert (bible-text bin (bible-select bin)))))

(defun sxx ()
  "Insert Bible Verse from the Septuagint"
  (interactive)
  (let ((bin "grb"))
    (insert (bible-text bin (bible-select bin)))))

(defun grb ()
  "Insert Bible Verse from the Septuagint"
  (interactive)
  (sxx))

(setq bible-minor-mode--bible-binary-list '("menge" "kjv" "vul" "grb"))

(defun bible-minor-mode--default-bible-binary ()
  "Default bible binary to use"
  (car bible-minor-mode--bible-binary-list))

(defun set-bible-version ()
  "Change the version of the bible"
  (interactive)
  (let ((s
         (ivy-read "Select bible version: " bible-minor-mode--bible-binary-list :require-match t :preselect (bible-minor-mode--default-bible-binary))))
    (setq bible-minor-mode--bible-binary-list (append `(,s) (delete s bible-minor-mode--bible-binary-list)))
  (message (bible-minor-mode--default-bible-binary))))

(defun cycle-bible-version ()
  (setq bible-minor-mode--bible-binary-list (append (cdr-safe bible-minor-mode--bible-binary-list) `(,(car bible-minor-mode--bible-binary-list)))))

;;; Bible Mode

(defun bible-mode--cycle-version ()
  "Cycles Bible Version used for currently displayed text"
  (interactive)
  (message "Hello")
  (cycle-bible-version)
  (let ((v (bible-mode--current-verse)))
    (message (format "-> %s" v))
    (bible-buffer-update (format "%s %s" bible-minor-mode--last-book
                                 bible-minor-mode--last-chapter-and-verse))
    (bible-mode--goto-verse v)))

(defun bible-mode--current-verse ()
  "Current Verse at point"
  (interactive)
  (message
  (save-excursion
    (car (s-split-up-to "\t" (thing-at-point 'line) 1))
    )))

(defun bible-mode--goto-verse (verse)
  (goto-char 0)
  (search-forward verse))

(define-derived-mode bible-mode text-mode (bible-minor-mode--default-bible-binary) "Major mode for reading the Bible.")

(map! :map bible-mode-map
      :n "x" #'bible-mode--cycle-version)

;;; Bible Minor Mode
(defun bible-tooltip (window object pos)
  ""
  (tooltip-mode 1)

  (let* ((start
          (save-excursion
            (goto-char pos)
            (while (not (eq 'Lu (get-char-code-property (char-after) 'general-category)))
              (backward-word))
            (let ((cbp (get-char-code-property (char-before) 'general-category))
                  (cond
                         ((t) (message (format "%s" cbp))
                         ))))))
         (verse
          (progn
            (string-match bible-minor-mode--bible-verse-regex (buffer-string) start)
            (match-string 0 (buffer-string)))))
    (message verse)
    (unless (and (boundp 'bible-minor-mode--last-verse) (string-equal bible-minor-mode--last-verse verse))
      (setq bible-minor-mode--last-verse verse)
      ;(run-with-idle-timer 10 nil (lambda () (setq bible-minor-mode--last-verse nil)))
      (let ((bible-verse (bible-text (bible-minor-mode--default-bible-binary) verse)))
        (if (> (length bible-verse) 2000)
            (substring bible-verse 0 2000)
          bible-verse)))))

(defun bible-show-verse ()
  "Shows tooltip for verse at point"
  (interactive)
  (setq bible-minor-mode--last-verse nil)
  (tooltip-show (bible-tooltip nil nil (point))))

(defun bible-minor-mode-highlight ()
  "Highlight bible verses"
  (font-lock-add-keywords nil `((,(bible-verse-regex) 0 '(face font-lock-keyword-face help-echo bible-tooltip))))
  (font-lock-flush))

(defun bible-minor-mode-revert ()
  "Undo highlight bible verses"
  (font-lock-remove-keywords nil `((,(bible-verse-regex) 0 '(face font-lock-keyword-face help-echo bible-tooltip))))
  (font-lock-flush))

;;;###autoload
(define-minor-mode bible-minor-mode
  "Highlight bible verses and provide tooltips for them"
  :lighter " ✝"
  :global nil
  (if bible-minor-mode
      (bible-minor-mode-highlight)
    (bible-minor-mode-revert)))

(add-hook 'org-mode-hook 'bible-minor-mode)


(defun bible-books-regex ()
  "Combines all bible books into a partial regex"
  (string-join (append (bible-books-short "kjv")
                       (bible-books-long "kjv")
                       (bible-books-short "menge")
                       (bible-books-long "menge"))
               "\\|"))


  "Creates a regex for verses of all bible books"
  (when (not (boundp 'bible-minor-mode--bible-verse-regex))
    (setq bible-minor-mode--bible-verse-regex
          (format
           "\\(%s\\) \\([[:digit:]]+\\(\\(\\:\\|\\,\\)[[:digit:]]+\\)?\\)\\(-[[:digit:]]+\\(\\(\\:\\|\\,\\)[[:digit:]]+\\)?\\)?"
           (bible-books-regex))))
  ;bible-minor-mode--bible-verse-regex)

(provide 'bible)
