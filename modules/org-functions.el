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
(defun org-insert-under-headline ()
  "Insert yanked text as last line under selected org headline."
  (interactive)
  (let (entries)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (derived-mode-p 'org-mode)
          (setq entries
                (nconc entries
                       (counsel-outline-candidates
                        (cdr (assq 'org-mode counsel-outline-settings))
                        (counsel-org-goto-all--outline-path-prefix)))))))
    (ivy-read "Goto: " entries
              :history 'counsel-org-goto-history
              :action #'org-insert-under-headline-helper
              :caller 'counsel-org-select-all)))

(defun org-insert-under-headline-helper (x)
  (save-window-excursion
  (org-goto-marker-or-bmk (cdr x))
  (outline-next-heading)
  (counsel-yank-pop)
  (newline)))

(provide 'org-functions)
