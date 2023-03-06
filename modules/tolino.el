(setq tolino-dir "/home/user/dox/tolino/")

(defun citar-move-to-tolino (keys-entries &optional str)
  "Search pdfs."
  (interactive (list (citar-select-refs)))
  (let ((files (hash-table-to-value-list
                (citar-get-files
                 keys-entries))))
  (cl-map 'vector #'(lambda (f) (copy-file f tolino-dir 1 t)) files)))

(provide 'tolino)
