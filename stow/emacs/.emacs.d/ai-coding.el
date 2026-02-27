(use-package aidermacs
  :custom
  (aidermacs-program '("aider"))
  (aidermacs-default-chat-mode 'code))

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist nil)
  (gptel-org-branching-context t)
  (gptel-model 'qwen3-coder-next)
  :config
  (setq gptel-expert-commands t)
  (setf (gptel-get-backend "ChatGPT") nil)
  (gptel-make-ollama "ollama"
    :host "localhost:11434"
    :stream t
    :models '(qwen3-coder-next)))
