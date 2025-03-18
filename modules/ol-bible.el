; Implements a link to a bible section or verse
                                        ; [[bible:Gen ̄1:1][Genesis 1:1]]
(load-module 'bible)
(load-module 'ol)

(org-link-set-parameters "bible"
                         :follow #'org-bible-open
                         :export #'org-bible-export
                         :store #'org-bible-store-link)


(define-derived-mode bible-view-mode org-mode "Bible View"
  "A mode to view the bible in.")

(map! :map bible-view-mode-map
      :nvi "ö" #'bible-view-cycle-language
      :nvi "x" #'bible-view-cycle-language)

(defun bible-view-cycle-language ()
  "Cycle the language in a bible view buffer."
  (interactive)
  (cycle-bible-version)
  (erase-buffer)
  (bible-insert (org-bible-get-chapter-verse)))



(defun org-bible-open (chapter-verse _)
  "Visit the bible page on CHAPTER-VERSE."
  (let ((buf (get-buffer-create (format "*Bible: %s*" chapter-verse))))
    (with-current-buffer buf
      (erase-buffer)
      (bible-view-mode)
      (bible-insert chapter-verse)
                                        ;(bible-mode 1)
      )
    (display-buffer buf '((display-buffer-pop-up-window))))
  )

;; (defun org-man-store-link (&optional _interactive?)
;;   "Store a link to a bible page."
;;   (when (memq major-mode '(bible-mode))
;;     ;; This is a man page, we do make this link.
;;     (let* ((page (org-man-get-page-name))
;;            (link (concat "man:" page))
;;            (description (format "Man page for %s" page)))
;;       (org-link-store-props
;;        :type "man"
;;        :link link
;;        :description description))))

(defun org-bible-get-chapter-verse ()
  "Extract the bible chapter and verse from the buffer name."
  ;; This works for `bible-view-mode'.
  (if
      (string-match "\\*Bible: \\(.*\\)\\*" (buffer-name))
      (match-string 1 (buffer-name))
    (error "Cannot find bible chapter"))
  )

(defun org-bible-export (link description format _)
  "Export a bible link from Org files."
  (let ((path (format "http://man.he.net/?topic=%s&section=all" link))
        (desc (or description link)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (_ path))))

(defun get-corresponding-element (element list1 list2)
  "Return the corresponding element in LIST2 for ELEMENT in LIST1."
  (let ((index (cl-position element list1 :test 'equal)))  ; Find the index of ELEMENT in LIST1
    (if index
        (nth index list2)  ; Return the corresponding element from LIST2
      nil)))  ; Return nil if ELEMENT is not found



(defun org-bible-insert ()
  "Insert a link to a bible chapter and verse."
  (interactive)

  (let* ((bb (bible-minor-mode--default-bible-binary))
         (pair (bible-select-internal bb))
         (book-long (car pair))
         (cv (car (cdr pair)))
         (book-short (get-corresponding-element book-long (bible-books-long bb) (bible-books-short bb))))
    (insert (format "[[bible:%s %s][%s %s]]" book-short cv book-long cv))))

(provide 'ol-bible)
