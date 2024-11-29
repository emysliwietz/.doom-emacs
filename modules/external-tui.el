;(setq ee-terminal-command "wezterm")
(require 'eee)
(setq ee-terminal-command "kitty")
(exwm-input-set-key (kbd "s-S-SPC")
                    'ee-yazi
                    )
(provide 'external-tui)
