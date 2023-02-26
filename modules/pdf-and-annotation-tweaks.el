(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   org-noter-notes-window-location 'horizontal-split
   ;; I want to see the whole file
   org-noter-hide-other nil
   org-noter-auto-save-last-location t
   org-noter-suggest-from-attachments t
   org-noter-doc-split-percentage '(0.7 . 0.3)
   org-noter-doc-split-fraction '(0.7 . 0.3)
   )

  (defun org-noter-insert-short-note ()
    "Insert note and switch focus back to pdf."
    (interactive)
    (save-window-excursion
    (org-noter-insert-note)))

  (defun pdf-view-theme-cycle ()
    "Cycle between emacs, midnight and white theme"
    (interactive)
    (cond
     ((bound-and-true-p pdf-view-themed-minor-mode)
      (progn
        (pdf-view-themed-minor-mode -1)
        (pdf-view-midnight-minor-mode 1)))
     ((bound-and-true-p pdf-view-midnight-minor-mode)
      (progn
        (pdf-view-midnight-minor-mode -1)
        (pdf-view-themed-minor-mode -1)))
     (t (progn
          (pdf-view-midnight-minor-mode -1)
          (pdf-view-themed-minor-mode 1)
          )))))

(after! pdf-view
  (evil-define-key 'normal pdf-view-mode-map "n" 'org-noter-insert-short-note)
  (evil-define-key 'normal pdf-view-mode-map "N" 'org-noter-insert-note)
  (bind-key (kbd "C-s") 'pdf-occur pdf-view-mode-map)
  (evil-define-key 'normal pdf-view-mode-map "r" 'pdf-view-rotate)
  (evil-define-key 'normal pdf-view-mode-map "i" 'pdf-view-theme-cycle)
  (add-hook 'pdf-view-hook 'pdf-view-themed-minor-mode))

(provide 'pdf-and-annotation-tweaks)
