(exwm-input-set-key (kbd "s-h") 'buf-move-left)
(exwm-input-set-key (kbd "s-l") 'buf-move-right)
(exwm-input-set-key (kbd "s-k") 'buf-move-up)
(exwm-input-set-key (kbd "s-j") 'buf-move-down)

; Resize
(exwm-input-set-key (kbd "s-<kp-8>") 'exwm-layout-enlarge-window)
(exwm-input-set-key (kbd "s-<kp-2>") 'exwm-layout-shrink-window)
(exwm-input-set-key (kbd "s-<kp-6>") 'exwm-layout-enlarge-window-horizontally)
(exwm-input-set-key (kbd "s-<kp-2>") 'exwm-layout-shrink-window-horizontally)
(exwm-input-set-key (kbd "s-<kp-5>") 'exwm-layout-toggle-mode-line)
(exwm-input--update-global-prefix-keys)
(provide 'exwm-buffer-movements)
