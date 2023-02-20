(setq org-roam-directory "~/sync/org/roam")
(mkdir org-roam-directory t)
(org-roam-db-autosync-mode 1)

;; Hide the mode line in the org-roam buffer, since it serves no purpose. This
;; makes it easier to distinguish among other org buffers.
(add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode)

;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package org-roam-protocol
  :after org-protocol)


(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq org-roam-bibtex-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))

;; Actually start using templates
 (after! org-capture
 ;; For browser capture
 (add-to-list 'org-capture-templates
 '("P" "Protocol" entry ; key, name, type
 (file+headline +org-capture-notes-file "Inbox") ; target
 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
 :prepend t ; properties
 :kill-buffer t))
(add-to-list 'org-capture-templates
'("L" "Protocol Link" entry
(file+headline +org-capture-notes-file "Inbox")
"* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n"
:prepend t
:kill-buffer t))
)

(use-package! org-roam-bibtex
  :after org-roam)

(provide 'org-roam-tweaks)
