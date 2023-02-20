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
<<<<<<< HEAD
 (evil-define-key 'normal pdf-view-mode-map "i" 'org-noter-insert-note))
=======
<<<<<<< HEAD
 (evil-define-key 'normal pdf-view-mode-map "i" 'org-noter-insert-note))
=======
  (evil-define-key 'normal pdf-view-mode-map "i" 'org-noter-insert-note)
 )
>>>>>>> f1a7c6092d9551dd9f063dbe9c86b63ce2f788bf
>>>>>>> 37a641d6a742f113ef8a5402ff1f766a52d6b231

(add-hook 'pdf-view-hook 'pdf-view-themed-minor-mode)
(provide 'pdf-and-annotation-tweaks)
