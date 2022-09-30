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

(defun bible-select (binary)
  "Ask user to select bible verse using kjv-like binary"
  (let* ((book (ivy-read "Select Book: " (bible-books-long binary)))
         (chapter-and-verse (ivy-read (format "Choose Chapter and Verse of %s: " book) '())))
    (format "%s %s" book chapter-and-verse)))

(defun bible-insert (&optional verse)
  "Inserts text of the Bible of selected verse"
  (interactive)
  (message verse)
  (if verse
      (insert (bible-text bible-minor-mode--default-bible-binary verse))
    (insert (bible-text bible-minor-mode--default-bible-binary (bible-select)))))

(defun bible ()
  "Open the Bible at selected verse"
  (interactive)
  (let ((bible-verse (bible-select bible-minor-mode--default-bible-binary)))
    (switch-to-buffer "*Bible*")
    (erase-buffer)
    (bible-insert bible-verse))
  (goto-char 0))


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
  (let ((bin "sxx"))
    (insert (bible-text bin (bible-select bin)))))

(defun grb ()
  "Insert Bible Verse from the Septuagint"
  (interactive)
  (sxx))

(setq bible-minor-mode--default-bible-binary "menge")
(defun set-bible-version ()
  "Change the version of the bible"
  (interactive)
  (setq bible-minor-mode--default-bible-binary
        (ivy-read "Select bible version: " '("kjv" "menge" "vul" "grb"))))

;;; Bible Mode
(defun bible-tooltip (window object pos)
  ""
  (tooltip-mode 1)
  (let* ((start
          (save-excursion
            (goto-char pos)
            (while (not (eq 'Lu (get-char-code-property (char-after) 'general-category)))
              (backward-word))
            (point)))
         (end
          (save-excursion
            (goto-char start)
            (re-search-forward "[^[:alnum:]\\:\\,\\-]" nil t 2)))
         (verse (substring-no-properties (buffer-string) (1- start) (- end 2))))
    (message verse)
    (unless (and (boundp 'bible-minor-mode--last-verse) (string-equal bible-minor-mode--last-verse verse))
      (setq bible-minor-mode--last-verse verse)
      ;(run-with-idle-timer 10 nil (lambda () (setq bible-minor-mode--last-verse nil)))
      (let ((bible-verse (bible-text bible-minor-mode--default-bible-binary verse)))
        (if (> (length bible-verse) 2000)
            (substring bible-verse 0 2000)
          bible-verse)))))

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

(defun bible-verse-regex ()
  "Creates a regex for verses of all bible books"
  (when (not (boundp 'bible-minor-mode--bible-verse-regex))
    (setq bible-minor-mode--bible-verse-regex
          (format
           "\\(%s\\) \\([[:digit:]]+\\(\\(\\:\\|\\,\\)[[:digit:]]+\\)?\\)\\(-[[:digit:]]+\\(\\(\\:\\|\\,\\)[[:digit:]]+\\)?\\)?"
           (bible-books-regex))))
  bible-minor-mode--bible-verse-regex)

(provide 'bible)
