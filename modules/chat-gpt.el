(use-package! gptel
  :defer t
  :config
    (secrets-get-secret-async "Passwords" "ChatGPT API Key" 'gptel-api-key)
    (setq gptel--system-message "")
  )
(provide 'chat-gpt)
