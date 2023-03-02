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

(map! :after pdf-view
      :map pdf-view-mode-map
      :n "n" 'org-noter-insert-short-note
      :n "N" 'org-noter-insert-note
      :n "r" 'pdf-view-rotate
      :n "i" 'pdf-view-theme-cycle
      :ne "<down-mouse-1>" 'ignore ; Because marking would reset rotation
      :n "+" 'pdf-enlarge
      :n "-" 'pdf-shrink)

(setq pdf-view-resize-factor (/ 5 3.0))

(defun pdf-shrink ()
  "Shrink a pdf in pdf-view.
I'm not sure why pdf-view-resize-factor isn't consistent in both directions, so I need two seperate factors."
  (interactive)
  (pdf-view-enlarge 0.6))

(defun pdf-enlarge ()
  "Enlarge a pdf in pdf-view.
I'm not sure why pdf-view-resize-factor isn't consistent in both directions, so I need two seperate factors."
  (interactive)
  (pdf-view-enlarge 0.8))

(after! pdf-view
  (bind-key (kbd "C-s") 'pdf-occur pdf-view-mode-map)
  (bind-key (kbd "<C-mouse-4>") 'pdf-enlarge pdf-view-mode-map)
  (bind-key (kbd "<C-mouse-5>") 'pdf-shrink pdf-view-mode-map)

  (add-hook 'pdf-view-hook 'pdf-view-themed-minor-mode))

(provide 'pdf-and-annotation-tweaks)
