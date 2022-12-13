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
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package vertico
  :bind (:map vertico-map
              ("M-RET" . minibuffer-force-complete-and-exit)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-d" . vertico-scroll-down)
              ("C-u" . vertico-scroll-up))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (setq history-length 100)
  (savehist-mode))


(defun first-initialism (pattern index _total)
  (if (= index 0) 'orderless-initialism))
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
   '((file (styles . (basic partial-completion)))
     (buffer (styles . (orderless basic)))))
  (orderless-matching-styles nil)
  (orderless-style-dispatchers '(regex-if-any-special))
  (orderless-component-separator " +\\|/\\|-"))

;; (setq read-file-name-completion-ignore-case t
;;       read-buffer-completion-ignore-case t
;;       completion-ignore-case t)
