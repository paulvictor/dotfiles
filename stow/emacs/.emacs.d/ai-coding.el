(use-package aidermacs
  :custom
  (aidermacs-program '("aider"))
  (aidermacs-default-chat-mode 'code))

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist nil)
  (gptel-org-branching-context t)
  ;;   (gptel-model 'qwen3-coder-next)
  :config
  (setf (gptel-get-backend "ChatGPT") nil)
  (setq gptel--known-backends nil
        gptel-expert-commands t
        gptel-backend (gptel-make-openai "xyne"
                        :host "grid.ai.juspay.net"
                        :key (password-store-get "work/Juspay/grid.ai/sarge")
                        :stream t
                        :endpoint "/v1/chat/completions"
                        :models '("minimaxai/minimax-m2"
                                  "glm-latest"
                                  "claude-opus-4-5"
                                  "kimi-latest")) )


  (gptel-make-ollama "local/anarki"
    :host "localhost:11434"
    :stream t
    :models '(qwen3-coder-next))
  (gptel-make-gemini "Gemini"
    :key (password-store-get "work/Juspay/gemini/anarki")
    :stream t)
  (gptel-make-openai "personal-openai"
    :key (password-store-get "openai/key/anarki")
    :endpoint "/v1/chat/completions"
    :stream t
    :models '(gpt-5-nano)))
