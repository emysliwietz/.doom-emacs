(global-set-key (kbd "C-ö") #'gptel-load-api-keys)
(defun gptel-load-api-keys ()
  "Query secret service for api keys"
  (interactive)
  (async-start
                                        ; Async function
   `(lambda ()
      (require 'secrets)
              (global-set-key (kbd "C-ö") #'gptel-send)
      (secrets-get-secret "Passwords" "Gemini API Key")
      )

                                        ; Callback
   `(lambda (result)
      (setq gptel-backend (gptel-make-gemini "Gemini" :key result :stream t)
            gptel--system-message ""
            gptel-model 'gemini-1.5-pro-latest
            ))))

(use-package! gptel
  :defer t
  :config
  (gptel-load-api-keys)
  (global-set-key (kbd "C-ö") #'gptel-send)
                                        ;(secrets-get-secret-async "Passwords" "ChatGPT API Key" 'gptel-api-key)
  )
(provide 'gpt-el-tweaks)
