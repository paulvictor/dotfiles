;;; ...  -*- lexical-binding: t -*-
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
    (seq-find (lambda (b) (string= b desired)) (mapcar #'buffer-name (buffer-list)))))

