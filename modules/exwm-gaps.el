(use-package! exwm-outer-gaps
  :after (exwm xelb)
  :config
  (exwm-outer-gaps-mode +1)
  (exwm-input-set-key (kbd "s-+") 'exwm-outer-gaps-increment)
  (exwm-input-set-key (kbd "s--") 'exwm-outer-gaps-decrement))
(provide 'exwm-gaps)
