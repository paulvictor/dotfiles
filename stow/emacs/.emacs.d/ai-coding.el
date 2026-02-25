(use-package aidermacs
  :custom
  (aidermacs-program '("aider"))
  (aidermacs-default-chat-mode 'code))

(use-package gptel
  :custom
  ((gptel-model 'qwen3-coder-next)
   (gptel-backend (gptel-make-ollama "Ollama"
                    :host "localhost:11434"
                    :stream t
                    :models '(qwen3-coder-next)))))
