;;; ol-man.el - Support for links to man pages in Org mode
(require 'ol)

(org-link-set-parameters "b"
                         :follow #'org-b-open
                         :export #'org-b-export
                         :store #'org-b-store-link)

(defun org-b-open (verse _)
  "Visit the verse."
  (funcall org-b-command path))

(defun org-b-store-link ()
  "Store a link to a man page."
  (when (memq major-mode '(Man-mode woman-mode))
    ;; This is a man page, we do make this link.
    (let* ((page (org-man-get-page-name))
           (link (concat "b:" page))
           (description (format "B page for %s" page)))
      (org-link-store-props
       :type "b"
       :link link
       :description description))))

(defun org-man-get-page-name ()
  "Extract the page name from the buffer name."
  ;; This works for both `Man-mode' and `woman-mode'.
  (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
      (match-string 1 (buffer-name))
    (error "Cannot create link to this man page")))

(defun org-b-export (link description format _)
  "Export a man page link from Org files."
  (let ((path (format "http://man.he.net/?topic=%s&section=all" link))
        (desc (or description link)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (t path))))

(provide 'org-links)
