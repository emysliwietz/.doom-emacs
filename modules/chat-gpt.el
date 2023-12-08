(setq gptel-api-key (secrets-get-secret "Passwords" "ChatGPT API Key"))
(setq gptel--system-message "")
(provide 'chat-gpt)
