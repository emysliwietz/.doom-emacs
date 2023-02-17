(defun org-copy-subtree-only ()
  "Copy the current subtree excluding heading and children into clipboard."
  (interactive)
  (if (org-before-first-heading-p)
      (message "Not in or on an org heading")
    (save-excursion
      ;; If inside heading contents, move the point back to the heading
      ;; otherwise `org-agenda-get-some-entry-text' won't work.
      (unless (org-on-heading-p) (org-previous-visible-heading 1))
      (let ((contents (substring-no-properties
                       (org-agenda-get-some-entry-text
                        (point-marker)
                        most-positive-fixnum))))
        (message "Copied %d chars" (length contents))
        (kill-new contents)))))

(defun add-filename-to-counsel-outline-candidates (candidates)
  "Add the filename at the beginning for CANDIDATES from `counsel-outline-candidates'."
  (mapcar
   (lambda (candidate)
     (let* ((marker (cdr candidate))
            (filename (buffer-file-name (marker-buffer marker)))
            (filename-abbreviated (when filename (concat (abbreviate-file-name filename) " ")))
            ;; Use this if you want the buffer name. It's a bit shorter.
            ;; (buffername (buffer-name (marker-buffer (cdr candidate))))
            )
       (cons (concat filename-abbreviated (car candidate)) marker)))
   candidates))

(advice-add 'counsel-outline-candidates :filter-return #'add-filename-to-counsel-outline-candidates)

(require 'counsel)
(require 's)

(defun org-get-headline-path (prompt)
  "Select a headline in any open org file and return marker to it."
  (let (entries)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (derived-mode-p 'org-mode)
          (setq entries
                (nconc entries
                       (counsel-outline-candidates
                        (cdr (assq 'org-mode counsel-outline-settings))
                        (counsel-org-goto-all--outline-path-prefix)))))))
    (let*
        ((sel
          (completing-read prompt entries nil t nil
                           'counsel-org-goto-history
                           ))
         (split (s-split-up-to " " sel 1))
         (filename (car split))
         (path (apply 's-split "/" (cdr split)))
         )
      (org-find-olp `(,filename ,@path))
      )))

(defun org-goto-headline ()
  "Go to any open org headline."
  (interactive)
  (org-goto-marker-or-bmk (org-get-headline-path "Goto: ")))


(defun org-insert-under-headline ()
  "Insert yanked text as last line under selected org headline."
  (interactive)
  (save-window-excursion
    (org-goto-marker-or-bmk
     (org-get-headline-path "Insert under: "))
    (outline-next-heading)
    (counsel-yank-pop)
    (newline)))

(defun org-config-new-block ()
  "Create a new code block belonging to specific module."
  (interactive)
  (let ((current-headline
         (string-inflection-title-to-lisp-case-function (nth 4 (org-heading-components)))))
    (insert (concat "*** Description\n#+begin_src emacs-lisp :tangle modules/" current-headline ".el\n\n#+end_src")))
  (evil-previous-visual-line))

(provide 'org-functions)
