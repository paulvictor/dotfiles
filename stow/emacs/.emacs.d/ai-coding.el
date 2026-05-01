(use-package aidermacs
  :custom
  (aidermacs-program '("aider"))
  (aidermacs-default-chat-mode 'code))

(defvar pvr/ai-backend-creds nil
  "alist of backend and credential cached after querying secure source(pass)")

(defvar pvr/ai-backend-passwd-store-entries
  '((xyne . "work/Juspay/grid.ai/sarge")
   (gemini . "work/Juspay/gemini/anarki")
   (personal-openai . "openai/key/anarki")))

(defun cache-get-passwd (backend passwd-store-key)
  (let* ((passwd-entry (password-store-get passwd-store-key))
         (assoc-entry (cons backend passwd-entry)))
    (!cons assoc-entry pvr/ai-backend-creds)
    passwd-entry))

(defun pvr/get-ai-backend-creds (backend)
  (if-let* ((pair (assoc backend pvr/ai-backend-creds)))
      (cdr pair)
    (if-let* ((entry (assoc backend pvr/ai-backend-passwd-store-entries)))
        (cache-get-passwd (car entry) (cdr entry))
      (error "backend %s error: Unable to get credentials" backend))))

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist nil)
  (gptel-org-branching-context t)
  :config
  (setq gptel--known-backends nil
        gptel-expert-commands t
        gptel-backend (gptel-make-openai "xyne"
                        :host "grid.ai.juspay.net"
                        :key (lambda () (pvr/get-ai-backend-creds 'xyne))
                        :stream t
                        :endpoint "/v1/chat/completions"
                        :models '("minimaxai/minimax-m2"
                                  "glm-latest"
                                  "claude-opus-4-5"
                                  "kimi-latest")))
  (gptel-make-ollama "private/anarki"
    :host "http://anarki:11434"
    :stream t
    :models '(qwen3-coder-next))
  (gptel-make-gemini "Gemini"
    :key (lambda () (pvr/get-ai-backend-creds 'gemini))
    :stream t)
  (gptel-make-openai "personal-openai"
    :key (lambda () (pvr/get-ai-backend-creds 'personal-openai))
    :endpoint "/v1/chat/completions"
    :stream t
    :models '(gpt-5-nano)))

(use-package agent-shell
  :config
  (defvar pvr/agent-shell-frame nil
    "The frame in which agent-shell was most recently launched.
Used by the `emax' Claude skill to target elisp eval at the right frame.")
  (add-hook 'agent-shell-mode-hook
            (lambda () (setq pvr/agent-shell-frame (selected-frame)))))

(defun pvr/clause-project-root ()
  "Return the first project root, or nil."
  (when-let ((project (project-current)))
    (project-root project)))

(defun pvr/clause-scope-label (label)
  "Compute the display label for BUFFER, scoped to the current project if any."
  (if-let ((root (pvr/clause-project-root)))
      (format "%s (%s)" label root)
    label))

(defun pvr/clause-existing-buffer (label)
  "Return an existing Claude buffer for LABEL, or nil."
  (let* ((display-label (pvr/clause-scope-label label))
         (desired (format "Claude [%s]" display-label)))
    desired
    (seq-find (lambda (b) (string= b desired)) (mapcar #'buffer-name (buffer-list)))))

(defun pvr/mk-agent-shell-claude (label auth env-vars)
  "Start a Claude Code agent pinned to a specific provider.
LABEL is used for the buffer name, scoped to the current project.
AUTH is the result of `agent-shell-anthropic-make-authentication'.
ENV-VARS is a flat list of alternating environment variable
name/value strings."
  (let* ((display-label (pvr/clause-scope-label label))
         (existing (pvr/clause-existing-buffer label)))
    (if existing
        (switch-to-buffer existing)
      (let ((agent-shell-anthropic-authentication auth)
            (agent-shell-anthropic-claude-environment
             (apply #'agent-shell-make-environment-variables
                    :inherit-env t env-vars)))
        (agent-shell--dwim
         :config (agent-shell-make-agent-config
                  :identifier 'claude-code
                  :mode-line-name (format "Claude [%s]" display-label)
                  :buffer-name (format "Claude [%s]" display-label)
                  :shell-prompt "Claude> "
                  :shell-prompt-regexp "Claude> "
                  :icon-name "claudecode.png"
                  :welcome-function #'agent-shell-anthropic--claude-code-welcome-message
                  :client-maker (lambda (buffer)
                                  (agent-shell-anthropic-make-claude-client :buffer buffer))
                  :install-instructions "See https://github.com/agentclientprotocol/claude-agent-acp for installation.")
         :new-shell t)))))

(defun pvr/claude-local ()
  (interactive)
  (pvr/mk-agent-shell-claude
   "local"
   (agent-shell-anthropic-make-authentication :api-key "ollama")
   (list "ANTHROPIC_BASE_URL"              "http://anarki:11434"
         "ANTHROPIC_MODEL"                 "qwen3-coder-next"
         "ANTHROPIC_DEFAULT_SONNET_MODEL"  "qwen3.6"
         "ANTHROPIC_DEFAULT_HAIKU_MODEL"   "gemma4"
         "ANTHROPIC_DEFAULT_OPUS_MODEL"    "qwen3-coder-next"
         "ANTHROPIC_SMALL_FAST_MODEL"      "deepseek-coder-v2"
         "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC" "1")))

(defun pvr/claude-anthropic ()
  (interactive)
  ;; Use login-based auth to bill against the Pro subscription, not per-token.
  (pvr/mk-agent-shell-claude
   "anthropic"
   (agent-shell-anthropic-make-authentication :login t)
   (list "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC" "1")))

(defun pvr/claude-work ()
  (interactive)
  (let ((key (cache-and-get-key "work/Juspay/grid.ai/sarge")))
    (pvr/mk-agent-shell-claude
     "juspay"
     (agent-shell-anthropic-make-authentication :api-key key)
     (list "ANTHROPIC_BASE_URL" "https://grid.ai.juspay.net"
           "ANTHROPIC_MODEL"    "minimaxai/minimax-m2"
           "ANTHROPIC_DEFAULT_SONNET_MODEL"     "minimaxai/minimax-m2"
           "ANTHROPIC_DEFAULT_HAIKU_MODEL"      "minimaxai/minimax-m2"
           "ANTHROPIC_DEFAULT_OPUS_MODEL"    "minimaxai/minimax-m2"
           "ANTHROPIC_SMALL_FAST_MODEL"    "minimaxai/minimax-m2"
           "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC" "1"))))

(defun pvr/claude-pick ()
  (interactive)
  (pcase (completing-read "Claude provider: " '("local" "anthropic" "juspay"))
    ("local"     (pvr/claude-local))
    ("anthropic" (pvr/claude-anthropic))
    ("juspay"      (pvr/claude-work))))
(use-package agent-shell-anthropic
 ;  :config
  ;; (let ((anthropic-key (cache-and-get-key "claude/api-key/key-1");"ollama";; (cache-and-get-key "work/Juspay/grid.ai/sarge")
;;                        ))
;;     (setq agent-shell-anthropic-authentication
;;           (agent-shell-anthropic-make-authentication :api-key anthropic-key)
;; ;;           agent-shell-anthropic-claude-environment
;;           ;; (agent-shell-make-environment-variables
;; ;;            :inherit-env t
;; ;;            "ANTHROPIC_BASE_URL"             "http://anarki:11434"
;; ;;            "ANTHROPIC_AUTH_TOKEN"           "ollama"
;; ;;            "ANTHROPIC_API_KEY"              "ollama"
;; ;;            "ANTHROPIC_DEFAULT_SONNET_MODEL" "qwen3-coder-next"
;; ;;            "ANTHROPIC_DEFAULT_HAIKU_MODEL"  "qwen3-coder-next"
;; ;;            "ANTHROPIC_DEFAULT_OPUS_MODEL"   "qwen3-coder-next"
;; ;;            "ANTHROPIC_SMALL_FAST_MODEL"     "qwen3-coder-next"
;; ;;            "ANTHROPIC_MODEL"                "qwen3-coder-next"
;; ;;            "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC" "1"
;; ;;            "CLAUDE_CODE_ATTRIBUTION_HEADER" "0")
;;           ))
                                        ;

  )
;; (agent-shell-make-environment-variables
;;            "ANTHROPIC_BASE_URL" "https://grid.ai.juspay.net"
;;            "CLAUDE_CODE_DISABLE_EXPERIMENTAL_BETAS" "1"
;;            "ANTHROPIC_SMALL_FAST_MODEL" "open-large"
;;            "ANTHROPIC_DEFAULT_HAIKU_MODEL" "open-large"
;;            "ANTHROPIC_DEFAULT_HAIKU_MODEL" "open-large"
;;            "ANTHROPIC_DEFAULT_SONNET_MODEL" "open-large")
