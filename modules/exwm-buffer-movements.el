(exwm-input-set-key (kbd "s-h") 'buf-move-left)
(exwm-input-set-key (kbd "s-l") 'buf-move-right)
(exwm-input-set-key (kbd "s-k") 'buf-move-up)
(exwm-input-set-key (kbd "s-j") 'buf-move-down)
(exwm-input--update-global-prefix-keys)
(provide 'exwm-buffer-movements)
