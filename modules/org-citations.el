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

(provide 'org-citations)
