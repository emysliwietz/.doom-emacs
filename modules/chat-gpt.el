(setq markdown-fontify-code-blocks-natively t)
(load-module 'async-await)
(defun gptel-gemini-load-api-key ()
  (async-start
   ; Async function
   `(lambda ()
      (require 'secrets)
      (secrets-get-secret "Passwords" "Gemini API Key")
      )

   ; Callback
   `(lambda (result)
      (setq
       gptel-model 'gemini-2.0-flash
       gptel-backend (gptel-make-gemini "Gemini"
                 :key result
                 :stream t))
      )
  )
  )
(use-package! gptel
  :defer t
  :config
    ;(secrets-get-secret-async "Passwords" "ChatGPT API Key" 'gptel-api-key)
  (gptel-gemini-load-api-key)
    ;(setq gptel--system-message "")
  )
(global-set-key (kbd "C-ö") 'gptel-send)
(provide 'chat-gpt)
