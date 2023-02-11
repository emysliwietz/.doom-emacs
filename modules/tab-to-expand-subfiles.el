(use-package! dired-subtree
  :defer t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))
(provide 'tab-to-expand-subfiles)
