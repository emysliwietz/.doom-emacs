(use-package! citar-embark
  :after citar embark
  :config (citar-embark-mode))

(use-package! citar
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  )

(after! citar
(setq citar-templates
      '((main . "${author editor:30}     ${date year issued:4}     ${title:80}")
        (suffix . "          ${=key= id:15}    ${=type=:10}    ${tags keywords:*}")
        (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
        (note . "Notes on ${author editor}, ${title}")))

(setq citar-symbols
      `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
        (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
        (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))))

(setq citar-symbol-separator " ")

(defun ex/search-pdf-contents (keys-entries &optional str)
  "Search pdfs."
  (interactive (list (citar-select-refs)))
  (let ((files (hash-table-to-value-list
                (citar-get-files
                 keys-entries)))
        (search-str (or str (read-string "Search string: "))))
    (pdf-occur-search files search-str t)))

(defun hash-table-to-value-list (hashtable)
  "Convert a hash table to a list of values"
  (let ((vlist '()))
    (maphash '(lambda (key value) (push value vlist)) hashtable)
    (flatten-list vlist)))

;; with this, you can exploit embark's multitarget actions, so that you can run `embark-act-all`
(after! embark
  (add-to-list 'embark-multitarget-actions #'ex/search-pdf-contents))

(setq zotra-cli-command '("node" "/home/user/dox/install/zotra-cli/index.js"))

(provide 'org-citations)
