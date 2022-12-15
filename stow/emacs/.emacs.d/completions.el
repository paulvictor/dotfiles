(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
        read-extended-command-predicate #'command-completion-default-include-p
        completions-detailed t)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(defun pvr/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

(use-package vertico
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ("M-RET" . minibuffer-force-complete-and-exit)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  (:map minibuffer-local-map
        ("<backspace>" . pvr/minibuffer-backward-kill))
  :custom
  (vertico-count 12)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (setq history-length 100)
  (savehist-mode))

(defun first-initialism (pattern index _total)
  (if (= index 0) 'orderless-initialism))

(defun pvr/orderless-literal-dispatch (pattern _index _total)
    (when (string-prefix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 1))))

(defun pvr/orderless-flex-dispatcher (pattern _index _total)
    (when (string-prefix-p "." pattern)
    `(orderless-flex . ,(substring pattern 1))))

(defun regex-if-any-special (pattern index tot)
  (cond
   ((string-prefix-p "RX" pattern) `(orderless-regexp . ,(substring pattern 2)))
   ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
   (t `(orderless-flex . pattern))))

;; TODO : Add a hydra to change between match dispatchers

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles . (partial-completion)))
     (buffer (styles . (orderless basic)))))
  (orderless-matching-styles nil)
  (orderless-style-dispatchers
   '(pvr/orderless-literal-dispatch
     pvr/orderless-flex-dispatcher
     regex-if-any-special))
  (orderless-component-separator 'orderless-escapable-split-on-space))

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; TODO : Add a keybinding to consult-ripgrep
;; consult-buffer instead counsel-switch-buffer
;; consult-project-root-function

;; Try embark
(use-package project
  :custom
  (project-switch-use-entire-map t)
  :config
  (general-define-key
   :keymaps 'project-prefix-map
   :prefix "C-c"
   "e" 'project-eshell
   "v" 'projectile-run-vterm)
  (general-define-key
   :keymaps 'project-prefix-map
   "b" 'consult-project-buffer
   "/" 'consult-ripgrep
   "g" 'magit))

(use-package consult
  :bind
  (("C-M-j" . persp-switch-to-buffer)
   :map minibuffer-local-map
        ("C-r" . consult-history))
  :general
  (:states 'normal
           "C-M-l"  #'consult-imenu
           "C-s" #'consult-line
           "C-x C-b" #'consult-buffer
           "C-y" #'consult-yank-from-kill-ring))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-on-exact-match 'insert)
  (corfu-auto nil) ; Only use `corfu' when calling `completion-at-point' or
                                        ; `indent-for-tab-command'

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-preselect-first t)

  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  :general
  (:keymaps 'corfu-map
            :states 'insert
            "C-j" #'corfu-next
            "C-k" #'corfu-previous
            "C-n" #'corfu-next
            "C-p" #'corfu-previous
            "<escape>" #'corfu-quit
            "<tab>" #'corfu-insert
            "<return>" #'corfu-insert
            "C-d" #'corfu-show-documentation)
  :config
  (evil-collection-corfu-setup)
  (setq tab-always-indent 'complete
        completion-cycle-threshold nil))

(use-package pcmpl-args)

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c c p" . completion-at-point) ;; capf
         ("C-c c t" . complete-tag)        ;; etags
         ("C-c c d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c c h" . cape-history)
         ("C-c c f" . cape-file)
         ("C-c c k" . cape-keyword)
         ("C-c c s" . cape-symbol)
         ("C-c c a" . cape-abbrev)
         ("C-c c i" . cape-ispell)
         ("C-c c l" . cape-line)
         ("C-c c w" . cape-dict))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))
