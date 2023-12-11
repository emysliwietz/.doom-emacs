(use-package! gptel
  :defer t
  :config
    (setq gptel-api-key (secrets-get-secret "Passwords" "ChatGPT API Key")
       gptel--system-message "")
  )
(provide 'chat-gpt)
