(setq completion-auto-wrap t
      ;;       completion-auto-help 'always
      completion-show-help nil
      completions-format 'one-column
      ;;       completion-auto-select t
      completion-auto-select 'second-tab
      completions-max-height 10
      tab-always-indent 'complete)

(defun pvr/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-delete-char arg)))

(use-package minibuffer
;;   :custom
;;   (completion-styles '(basic partial-completion emacs22 initials))
;;   :config
  ;;   (keymap-unset minibuffer-mode-map "<space>" t)
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-cycle-threshold 3)
  :bind (:map minibuffer-mode-map
              ("<tab>" . minibuffer-next-completion)
              ("S-<tab>" . minibuffer-previous-completion)))

(use-package vertico
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ("M-RET" . minibuffer-force-complete-and-exit))
  (:map minibuffer-local-map
;;         ("<backspace>" . pvr/minibuffer-backward-kill)
        ("C-w" . backward-kill-word))
  :custom
  (vertico-count 12)
  (vertico-cycle t)
  :init
  (vertico-mode 1))

(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode 1))

(use-package savehist
  :init
  (setq history-length 100)
  (savehist-mode 1))

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
   ((string-prefix-p "~~" pattern) `(orderless-regexp . ,(substring pattern 2)))
   ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
   (t `(orderless-flex . pattern))))

;; TODO : Add a hydra to change between match dispatchers

(use-package orderless
  :custom
  (completion-styles '(orderless basic partial-completion))
  (completion-category-overrides
   '((file (styles . (partial-completion)))
     (buffer (styles . (orderless basic partial-completion)))))
  (orderless-matching-styles nil)
  (orderless-style-dispatchers
   '(pvr/orderless-literal-dispatch
     pvr/orderless-flex-dispatcher
     regex-if-any-special))
  (orderless-component-separator 'orderless-escapable-split-on-space))

;; TODO : Add a keybinding to consult-ripgrep
;; consult-buffer instead counsel-switch-buffer
;; consult-project-root-function

;; Try embark
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer ; Overridden by bufler-switch-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump

         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-r" . consult-history)
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package corfu
  :init
  (global-corfu-mode)
  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :custom
  (corfu-cycle t)
  (corfu-on-exact-match 'insert)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-preselect 'prompt)
  (corfu-preview-current 'insert))

;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-use-icons t)
;;   (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
;;   (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
;;   (kind-icon-blend-frac 0.08)

;;   ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
;;   ;; directory that defaults to the `user-emacs-directory'. Here, I change that
;;   ;; directory to a location appropriate to `no-littering' conventions, a
;;   ;; package which moves directories of other packages to sane locations.
;;   (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'
;;   )

(use-package pcmpl-args
  :after pcomplete)

(defun buffers-with-same-major-mode ()
  (interactive)
  (let ((current-major-mode major-mode))
    (seq-filter
     (lambda (b)
       (with-current-buffer b
         (eq major-mode current-major-mode)))
     (buffer-list))))

(setq non-lisp-prog-modes
      '(haskell-mode nix-mode javascript-mode java-mode json-mode shell-mode))

(use-package cape
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local
               completion-at-point-functions `(,(cape-capf-super #'elisp-completion-at-point #'cape-dabbrev) cape-file)
               cape-dabbrev-min-length 3)))
  (dolist (mode non-lisp-prog-modes)
    (add-hook (derived-mode-hook-name mode)
              (lambda ()
                (setq-local
                 completion-at-point-functions `(,(cape-capf-super #'cape-dabbrev #'cape-keyword))))))
;;   (add-hook 'prog-mode-hook
;;             (lambda ()
;;               (setq-local completion-at-point-functions
;;                           (list #'cape-dabbrev-dict-keyword))))
  ;; For pcomplete. For now these two advices are strongly recommended to
  ;; achieve a sane Eshell experience. See
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-shell-or-eshell

  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  )


