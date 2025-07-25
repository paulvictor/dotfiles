;;; init.el -*- lexical-binding: t; -*-

(require 'package)
(require 'man)
(require 'ffap)
;; Setup straight.el
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name
;;         "straight/repos/straight.el/bootstrap.el"
;;         (or (bound-and-true-p straight-base-dir)
;;             user-emacs-directory)))
;;       (bootstrap-version 7))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
;; (straight-use-package 'use-package)

;;  (setq straight-use-package-by-default t) ;; like setting :straight t forall use-package forms

(use-package package
  :config
  (package-initialize))

(use-package f)
(setq visual-bell t)
(setq visible-bell t)
(setq whitespace-style '(trailing tabs))
(column-number-mode)
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook
  (lambda ()
    (whitespace-cleanup)))
(defun set-env-vars ()
  (setenv "SSH_AUTH_SOCK"
          (substring
           (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket") 0 -1))
  (setenv "EDITOR" "emacsclient -c"))

(defvar pvr/persist-dir
  (let ((d (or (getenv "PERSIST_DIR") "~/plain")))
    (unless (f-dir? d)
      (error "PERSIST_DIR not set"))
    d))

(defvar pvr/emacs-persist-dir
  (f-join pvr/persist-dir "emacs.d"))
(use-package s)
(use-package dash)

;;Do not save duplicates in kill ring
(setq kill-do-not-save-duplicates t)
(mapc (lambda (m)
        (add-hook m
                  (lambda ()
                    (setq-local tab-width 2))
                  ))
      '(c-mode-hook haskell-mode-hook nix-mode-hook))

(use-package emacs
  :custom
  (tab-width 2)
  ;; Enable recursive minibuffers
  (enable-recursive-minibuffer t)
  (cursor-type 'bar)
  (tab-always-indent 'complete)
  :init
  ;; Default modes to start by default
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (winner-mode 1))

(use-package repeat
  :hook (after-init . repeat-mode)
  :custom
  (repeat-exit-key "<return>")
  (repeat-exit-timeout 5))

(use-package pulsar
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.05)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-yellow)

  (pulsar-pulse-functions
   '(recenter-top-bottom
     move-to-window-line-top-bottom
     reposition-window
     bookmark-jump
     other-window
     delete-window
     delete-other-windows
     forward-page
     backward-page
     scroll-up-command
     scroll-down-command
     windmove-right
     windmove-left
     windmove-up
     windmove-down
     windmove-swap-states-right
     windmove-swap-states-left
     windmove-swap-states-up
     windmove-swap-states-down
     tab-new
     tab-close
     tab-next
     org-next-visible-heading
     org-previous-visible-heading
     org-forward-heading-same-level
     org-backward-heading-same-level
     outline-backward-same-level
     outline-forward-same-level
     outline-next-visible-heading
     outline-previous-visible-heading
     outline-up-heading
     eshell-previous-prompt
     eshell-next-prompt))
  :config
  (pulsar-global-mode 1)
  (setq isearch-update-post-hook
        '(pulsar-pulse-line)))

(setq initial-scratch-message nil)
;; Repeat maps to repeat commands. Check describe-repeat-modes
;; Set minibuffer completion history length to 10000
(setq history-length 10000)
(setq suggest-key-bindings t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(use-package no-littering
  :init
  (setq no-littering-etc-directory
        (f-join pvr/emacs-persist-dir "emacs/")
        no-littering-var-directory
        (f-join pvr/emacs-persist-dir "var/")))

(defun init-dashboard ()
  (require 'dashboard) ; Hack to get dashboard loaded
  (switch-to-buffer "*dashboard*")
  (dashboard-mode)
  (dashboard-insert-startupify-lists)
  (dashboard-refresh-buffer))

(defun pvr/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
           (image-path (concat user-emacs-directory "emacs-e-template.svg"))
           (image (create-image image-path))
           (size (image-size image))
           (height (cdr size))
           (width (car size))
           (top-margin (floor (/ (- (window-height) height) 2)))
           (left-margin (floor (/ (- (window-width) width) 2)))
           (prompt-title "Welcome to Emacs!"))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
;;       (insert (make-string (floor (/ (- (window-width) (string-width prompt-title)) 2)) ?\ ))
      ;; (insert prompt-title)
      )
    (setq cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (current-buffer)))

(defun pvr/set-font-faces ()
  (set-mouse-color "white")

  ;;   (set-face-attribute 'default nil :family "UDEV Gothic 35NFLG" :height 110)
  ;;   (set-face-attribute 'default nil :family "Monoid Nerd Font" :height 110)

;;   (set-face-attribute 'default nil :family "VictorMono Nerd Font" :height 110 :weight 'bold)
;;   (set-face-attribute 'term nil :family "IosevkaTerm Nerd Font Mono" :height 60)
;;   (set-face-attribute 'fixed-pitch nil :font "Iosevka Fixed Slab" :height 110 :weight 'bold)
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 120)
;;     (set-face-attribute 'default nil :family "DejaVu Serif" :height 110)

  ;;   (set-face-attribute 'default nil :family "JuliaMono" :height 110)

  (set-face-attribute 'bold nil :weight 'bold)
  (set-face-attribute 'italic nil :slant 'oblique)
  (set-fontset-font t nil "Symbols Nerd Font" nil 'append)

  (set-frame-parameter (selected-frame) 'alpha '(90 . 90)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (pvr/set-font-faces)
                  (set-env-vars)
                  (setq initial-buffer-choice #'pvr/show-welcome-buffer))))
  (progn
    (pvr/set-font-faces)
    (setq initial-buffer-choice #'pvr/show-welcome-buffer)))

(add-hook
  'prog-mode-hook
  #'(lambda ()
    (setq show-trailing-whitespace t)))

;Show matching parens
(show-paren-mode t)

(setq auto-save-timeout nil)
(setq make-backup-files nil)
(setq tab-width 2)
(setq bookmark-save-flag 1)
(setq bookmark-use-annotations t)
(setq bookmark-size-search 100)
;; (global-display-line-numbers-mode t)
(setq comment-style "aligned")
;; TODO : Save it in a proper sync'able place
;; (setq savehist-file "~/git/.emacs.d/personal/emacs-history")

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)
;; keymap-set and keymap-unset to bind keys in keymaps

;; Auto-revert to disk on file change
(global-auto-revert-mode 1)

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)    ; if nil, bold is universally disbabled
  (doom-themes-enable-italic t)
  :config
  (doom-themes-org-config)
  (load-theme 'doom-tomorrow-night t))

(use-package doom-modeline
  :custom
  (doom-modeline-vcs-icon t)
  (doom-modeline-minor-modes t)
  (doom-modeline-window-width-limit 100)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-buffer-encoding t)
  (doom-modeline-modal-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-persp-name t)
  (doom-modeline-display-default-persp-name nil)
  (doom-modeline-persp-icon t)
  :config
  (doom-modeline-mode 1))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line.
Also move to the next line, since that's the most frequent action after"
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg))
  (next-line))

(global-set-key  (kbd "M-;") #'comment-dwim-line)

(use-package which-key
  :custom
  (which-key-show-docstrings t)
  (which-key-show-prefix 'mode-line)
  (which-key-idle-delay 0.2)
  ;; max width of which-key frame: number of columns (an integer)
  (which-key-frame-max-width 60)
  ;; max height of which-key frame: number of lines (an integer)
  (which-key-frame-max-height 20)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :hook (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved for `vertico-repeat'
  :bind
  (;; ("M-." . #'vertico-repeat) ; Perfectly return to the state of the last Vertico minibuffer usage
   ;;    ("C-." . #'vertico-repeat-select)
   :map vertico-map
   ("M-<RETURN>" . vertico-exit-input)
   ("C-M-n" . #'vertico-next-group)
   ("C-M-p" . #'vertico-previous-group))
  :custom
  (vertico-multiform-commands
   '((consult-imenu buffer indexed)
     (consult-ripgrep buffer)
     (consult-grep buffer)
     (file grid)
     (execute-extended-command reverse)))
  (vertico-cycle t))

;;(use-package vertico-buffer)
(use-package savehist
  :config
  (setq savehist-file (no-littering-expand-var-file-name "savehist") )
  (savehist-mode 1)
  (setq history-length t
        history-delete-duplicates t
        savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring))
  :init
  (savehist-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font"))


;; Commenting to get it running
(use-package nerd-icons-completion
  :init
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package project
  :custom
  (project-switch-use-entire-map t)
  (project-key-prompt-style 'brackets)
  :config
  (general-define-key
   :keymaps 'project-prefix-map
   "b" 'consult-project-buffer
   "/" 'consult-ripgrep
   "g" 'magit))

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
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
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
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
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

;; (use-package recentf
;;   :config
;;   (recentf-mode t)
;;   (setq recentf-max-saved-items 500))

(use-package isearch
  :bind
  (:map isearch-mode-map
        ([remap isearch-delete-char] . isearch-del-char))
  :config
  (progn
    (require 'casual-isearch)
    (keymap-set isearch-mode-map "C-M-g" #'casual-isearch-tmenu))
;;   :config
;;   ;; Prevents issue where you have to press backspace twice when
;;   ;; trying to remove the first character that fails a search
;; ;;   (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;;   (defadvice isearch-search (after isearch-no-fail activate)
;;     (unless isearch-success
;;       (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
;;       (ad-activate 'isearch-search)
;;       (isearch-repeat (if isearch-forward 'forward))
;;       (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
;;       (ad-activate 'isearch-search)))
  :custom
  (isearch-allow-motion t)
  (isearch-repeat-on-direction-change t)
  (isearch-lazy-count t)
  (isearch-wrap-pause 'no-ding)
  (search-ring-max 100)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil))

(use-package wgrep) ;; TODO fix the configs
(use-package pdf-tools
  :config
    (pdf-tools-install t t nil nil)
    (setq-default pdf-view-display-size 'fit-width)
    (add-hook 'pdf-view-mode-hook
      (lambda ()
        (blink-cursor-mode -1)))
  :custom
    (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(use-package replace
  :custom
  (list-matching-lines-default-context-lines 2))

(use-package eldoc
  :hook
  ;; enable eldoc for minibuffer evaluations use this snippet
  ;; (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (eval-expression-minibuffer-setup . eldoc-mode))

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (thing-at-point 'symbol))
        regexp-history)
  (call-interactively 'occur))

(bind-key "M-s o" 'occur-dwim)

(defun add-to-words-syntax (mode-hook chars)
  (seq-do
    #'(lambda (c)
       (add-hook mode-hook
        #'(lambda () (modify-syntax-entry c "w"))))
   chars))

(add-to-words-syntax 'emacs-lisp-mode-hook "-_")
(add-to-words-syntax 'nix-mode-hook "-_")
;; (add-to-words-syntax 'haskell-mode-hook "_")
(add-to-words-syntax 'org-mode-hook "_-")
(add-to-words-syntax 'ess-r-mode-hook "_")
(add-to-words-syntax 'lisp-mode-hook "_-")
;; (add-to-words-syntax 'c++-mode-hook "_")
;; (add-to-words-syntax 'sh-mode-hook "_-")

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind
  (("C-x D" . dired-jump))
  :custom
  ((dired-listing-switches "-agho --group-directories-first")
   (dired-isearch-filenames 'dwim)
   (dired-kill-when-opening-new-dired-buffer t)
   (dired-create-destination-dirs t)
   (dired-dwim-target t))
  :config
  (progn
    (require 'casual-dired)
    (keymap-set dired-mode-map "C-M-g" #'casual-dired-tmenu)))

(use-package dired-single
  :after (dired dired-jump)
  :bind
  ([remap dired-find-file] . dired-single-buffer)
  ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
  ([remap dired-up-directory] . dired-single-up-directory)
  :commands (dired dired-jump))

(defun pvr/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(use-package ibuffer
  :hook
  (ibuffer-mode . hl-line-mode)
  :config
  (progn
    (require 'casual-ibuffer)
    (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
    (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
    (keymap-set ibuffer-mode-map "C-M-g" #'casual-ibuffer-tmenu))
  :custom
  (ibuffer-movement-cycle nil)
  (ibuffer-default-shrink-to-minimum-size nil)
  (ibuffer-formats
   '((mark modified read-only locked
           " "
           (name 40 40 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))
  (ibuffer-saved-filter-groups nil)
  (ibuffer-old-time 24))

(defun pvr/create-or-switch-perspective (dir)
  (persp-switch (f-filename dir)))

(use-package perspective
  :after project
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-initial-frame-name "Main")
  :config
  (advice-add 'project-switch-project :before #'pvr/create-or-switch-perspective)
  (persp-mode 1))

(use-package magit
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-diff-refine-hunk t))

(defun sm-try-smerge ()
  (save-excursion
 	  (goto-char (point-min))
 	  (when (re-search-forward "^<<<<<<< " nil t)
 	    (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)

(defun hyperspec-lookup--hyperspec-lookup-w3m (orig-fun &rest args)
  (let ((browse-url-browser-function 'w3m-browse-url))
    (apply orig-fun args)))

;; (use-package slime
;;   :hook (lisp-mode . slime-mode)
;;   :init
;;   (setq inferior-lisp-program "sbcl") ; TODO : Move to dir specific config
;; ;;      (add-to-list 'slime-contribs 'slime-fancy)
;;   :config
;;   (advice-add 'hyperspec-lookup :around #'hyperspec-lookup--hyperspec-lookup-w3m)
;;   (add-hook 'slime-load-hook
;;             (lambda ()
;;               (define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup)))
;;   (require 'slime-autoloads))

(use-package sly
;;   :hook
;;   (lisp-mode . sly-mode)
  :custom
  (inferior-lisp-program "sbcl"))

(use-package ielm
  :bind
  (:map inferior-emacs-lisp-mode-map
        ("C-l" . comint-clear-buffer)
        ("C-r" . consult-history))
  :init
  (let
      ((history-path (no-littering-expand-etc-file-name "ielm/history")))
    (make-directory (file-name-directory history-path) t)
    (add-hook 'ielm-mode-hook #'(lambda ()
                                  (setq-local comint-input-ring-file-name history-path)
                                  (setq-local comint-input-ring-size 10000)
                                  (setq-local comint-input-ignoredups t)
                                  (comint-read-input-ring)))
    (advice-add 'ielm-send-input :after #'(lambda (&rest args)
                                            (with-file-modes #o600
                                              (comint-write-input-ring))))))

(use-package emacs-slime-nav
  :hook
  ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1))

(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(f-join pvr/emacs-persist-dir "undo")))
   (undo-tree-visualizer-timestamps t)
   (undo-tree-visualizer-diff t))
  :bind (("C-x u" . undo-tree-undo)
         ("C-x U" . undo-tree-visualize))
  :config
  (global-undo-tree-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package expand-region
  :config
  (global-set-key (kbd "C-'") 'er/expand-region)
  (global-set-key (kbd "C-\"") 'er/contract-region)
  (set-variable 'expand-region-subword-enabled t))

(setq home-row-keys
      (string-to-list "neio"))

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (activate-markring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

(use-package avy
  :bind
  (("M-g c" . avy-goto-char-2)
   ("M-g l" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-l" . avy-goto-char-timer))
  :custom
  (avy-keys home-row-keys)
  (avy-background t)
  (avy-styles-alist
   '((avy-goto-char-2 . post)
     (avy-goto-line . pre)
     (avy-goto-char-timer . at-full)))
  :config
  (progn
    (require 'casual-avy)
    (keymap-global-set "C-M-g" #'casual-avy-tmenu))
  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char))

(use-package avy-zap
  :bind
  (("M-z" . avy-zap-to-char)
   ("C-z" . avy-zap-up-to-char))
  :init
  (setq avy-zap-dwim-prefer-avy t))

(use-package ace-window
  :bind
  (("M-o" . ace-window))
  :custom
  (aw-keys home-row-keys)
  (aw-scope 'frame)
  (aw-ignore-current nil))

(use-package hydra)

(defun eshell-persp ()
  "Crete a new persp for eshell"
  (interactive)
  (persp-switch "eshell")
  (with-perspective "eshell"
    (let
        ((buffer (eshell)))
      (persp-switch-to-buffer buffer)
      (persp-set-buffer "*eshell*"))))

(defun vterm-persp ()
  "Crete a new persp for vterm"
  (interactive)
  (persp-switch "vterm")
  (with-perspective "vterm"
    (let
        ((buffer (vterm)))
      (persp-switch-to-buffer buffer)
      (persp-set-buffer "*vterm*"))))

(use-package purescript-mode
  :mode "\\.purs\\'")

(use-package psc-ide
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (psc-ide-flycheck-setup)
              (turn-on-purescript-indentation))))

(defun pvr/new-eshell-window ()
  (interactive)
  (let* ((name (pvr/random-name)))
    (progn
      (let ((shell-buffer (eshell "new")))
        (rename-buffer (concat "*EsHeLl: " name "*"))
        (switch-to-buffer shell-buffer)))))

;; keep this as last as possible after all the minor modes
;; (add-hook 'after-init-hook #'envrc-global-mode)

(use-package direnv
  :config
  (direnv-mode))

(use-package popper
  :bind (("M-`" . popper-cycle)
         ("C-`" . popper-toggle-latest)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-group-function #'popper-group-by-project)
  :init
  (setq popper-window-height 25)
  ;; Match eshell, shell, term and/or vterm buffers
  (setq popper-reference-buffers
        '(          ;"^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$" shell-mode  ;shell as a popup
          "^\\*term.*\\*$" term-mode    ;term as a popup
          "^\\*vterm.*\\*$" vterm-mode  ;vterm as a popup
          helpful-mode
          nix-repl-mode
          compilation-mode
          "\\*Messages\\*"))
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")

(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build nix-eshell nix-eshell-with-packages nix-shell))

(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))

(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package electric
  :config
  (electric-indent-mode 1))

(use-package org
  :custom
  (org-special-ctrl-a/e t)
  (org-return-follow-links t)
  (org-M-RET-may-split-line
   '((headline . nil)
     (item . nil)
     (default . t)))
  :bind (:map org-mode-map
              ("C-M-i" . completion-at-point)))

(use-package org-tree-slide
  :bind (:map org-tree-slide-mode-map
              ("<right>" . org-tree-slide-move-next-tree)
              ("<left>" . org-tree-slide-move-previous-tree)
              ("l" . org-tree-slide-move-next-tree)
              ("h" . org-tree-slide-move-previous-tree))
  :init
  (setq org-tree-slide-play-hook
        #'(lambda ()
            (setq-local visual-fill-column-center-text t)
            (setq-local visual-fill-column-extra-text-width (cons 10 10))
            (visual-fill-column-mode 1)))
  (setq org-tree-slide-stop-hook
        #'(lambda ()
            (visual-fill-column-mode -1)))
  (add-hook 'org-tree-slide-mode #'(lambda ()))
  :custom
  (org-image-actual-width nil))

(use-package erc
  :custom
  (erc-server "irc.libera.chat")
  (erc-nick "paulvictor")
  (erc-user-full-name "Paul Victor")
  (erc-autojoin-channels '(("irc-libera.chat" "#systemcrafters" "#emacs")))
  (erc-kill-buffer-on-part t))

(use-package geiser
  :config
  ;; (setq geiser-default-implementation 'gambit)
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile chez))
  (setq geiser-implementations-alist
        '(((regexp "\\.scm$") guile)
         ((regexp "\\.def\\'") chez) ((regexp "\\.ss\\'") chez)
         ((regexp "\\.rkt$") racket))))

(load-file (concat user-emacs-directory "ghcid.el"))
(load-file (concat user-emacs-directory "utils.el"))
(load-file (concat user-emacs-directory "completions.el"))
(load-file (concat user-emacs-directory "names.el"))
(load-file (concat user-emacs-directory "eshell.el"))

(setq display-buffer-alist
      (append display-buffer-alist
      '(

        ;; The added space is for didactic purposes

        ;; Each entry in this list has this anatomy:

        ;; ( BUFFER-MATCHING-RULE
        ;;   LIST-OF-DISPLAY-BUFFER-FUNCTIONS
        ;;   OPTIONAL-PARAMETERS)

        ;; Match a buffer whose name is "*Occur*".  We have to escape
        ;; the asterisks to match them literally and not as a special
        ;; regular expression character.
        ("\\*\\(Help\\|Occur\\|cider-apropos\\|cider-doc\\)\\*"
         ;; If a buffer with the matching major-mode exists in some
         ;; window, then use that one.  Otherwise, display the buffer
         ;; below the current window.
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         ;; Then we have the parameters...
         (window-height . 0.3)
         (dedicated . t)))))


(defun indent-between-pair (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-package smartparens-config
  :custom
  (sp-base-key-bindings 'paredit)
  (sp-override-key-bindings
   '(("M-s" . nil)
     ("M-r" . nil)
     ("C-M-s" . sp-splice-sexp) ;; This removes the regex isearch which can strill be gotten with M-s r in isearch
     ("C-M-r" . sp-raise-sexp)
     ("C-M-j" . sp-join-sexp)
     ("C-M-J" . sp-split-sexp)))
  :config
  (show-smartparens-global-mode 1)
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET"))))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'geiser-repl-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'ielm-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'sly-mrepl-hook 'turn-on-smartparens-strict-mode)

;; Move cursor to end of current line
;; Insert new line below current line
;; it will also indent newline
(defun add-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun add-line-above ()
  (interactive)
  ;; FIXME Doesnt work at the beginning of the buffer
  (previous-line)
  (end-of-line)
  (newline-and-indent))

;; Move cursor to previous line
;; Go to end of the line
;; Insert new line below current line (So it actually insert new line above with indentation)
;; it will also indent newline
;; Disabling this as it conflicts with org mode
(add-hook ;; Can we use only the balancing feature in c mode
  'prog-mode-hook
  #'(lambda ()
      (local-set-key (kbd "C-<return>" ) 'add-line-below)
      (local-set-key (kbd "M-<return>") 'add-line-above)))

(global-set-key (kbd "C-x k") #'kill-current-buffer)
(global-set-key (kbd "C-<tab>") #'next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") #'previous-buffer)

(put 'upcase-region 'disabled nil)

(use-package iedit
  :custom
  (iedit-toggle-key-default (kbd "C-:")))

(use-package proced
  :custom
  (proced-auto-update-interval 2)
  (proced-auto-update-flag t)
  (proced-format 'medium))

;; From https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))
;; This is a commen
;; Really this works with
;; M-j
(global-set-key (kbd "C-^") 'top-join-line)

;; https://amitp.blogspot.com/2007/03/emacs-move-autosave-and-backup-files.html
(let
    ((user-temp-file-directory (f-join temporary-file-directory user-login-name "emacs")))
  (f-mkdir-full-path user-temp-file-directory)
  (setq backup-by-copying t)
  (setq backup-directory-alist
        `(("." . ,user-temp-file-directory)
          (,tramp-file-name-regexp nil)))
  (setq auto-save-list-file-prefix
        (concat user-temp-file-directory ".auto-saves-"))
  (setq auto-save-file-name-transforms
        `((".*" ,user-temp-file-directory t))))

(use-package nov
  :custom
  (nov-variable-pitch nil)
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package info
  :config
  (progn
    (require 'casual-info)
    (keymap-set Info-mode-map "C-M-g" #'casual-info-tmenu))
  :custom
  (Info-isearch-search nil))

;; (use-package ts-fold
;;   :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

;; (straight-use-package
;;  '(webkit :type git :host github :repo "akirakyle/emacs-webkit"
;;           :branch "main"
;;           :files (:defaults "*.js" "*.css" "*.so")
;;           :pre-build ("make")))


;; (use-package exwm
;;   :init
;;   (require 'exwm-config)
;;   (setq exwm-workspace-number 5)
;;   (add-hook 'exwm-update-class-hook #'(lambda ()
;;                                         (exwm-workspace-rename-buffer exwm-class-name)))
;;   (setq exwm-input-global-keys
;;         `(([?\s-w] . exwm-workspace-switch)
;;           ([?\s-r] . exwm-reset)
;;           ([?\s-h] . windmove-left)
;;           ([?\s-j] . windmove-down)
;;           ([?\s-k] . windmove-up)
;;           ([?\s-l] . windmove-right)
;;           ((kbd "s-<return>") . (lambda () (eshell)))
;;           ,@(mapcar (lambda (i)
;;                       `(,(kbd (format "s-%d" i)) .
;;                         (lambda ()
;;                           (interactive)
;;                           (exwm-workspace-switch-create ,i))))
;;                     (number-sequence 0 9))
;;           ([\?s-&] . (lambda (command)
;;                        (interactive (list (read-shell-command "$ ")))
;;                        (start-process-shell-command command nil command)))))
;;   :config
;;   (require 'exwm-randr)
;;   (with-eval-after-load 'exwm-randr
;;     (exwm-enable)
;;     (exwm-randr-mode 1)))


(use-package ngnk-mode)
(use-package ngnk-cli)
;; One thing is that C-. is not bound to anythng and can be used as a prefix
(use-package org-roam
  :custom
  (org-roam-directory "~/org-roam-notes/")
  (org-roam-completion-everywhere t)
  (org-roam-mode-sections '((org-roam-backlinks-section :unique t)
                            org-roam-reflinks-section))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c r t" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r c" . org-roam-capture)
         ("C-c r i" . org-roam-node-insert))
  :config

  (org-roam-setup))


(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))

(use-package browse-url
  :custom
;;   (browse-url-browser-function #'browse-url-firefox) ;; We cant set this up properly yet because we only have firefox-devedition. If we can create a symlink, would solve
  (browse-url-firefox-new-window-is-tab t))

(use-package org-download
  :hook
  ((org-mode dired-mode) . org-download-enable)
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method "grim + slurp"))

(use-package hydra)
(global-set-key (kbd "C-c C-v")
                (defhydra hydra-actions ()
                  "Misc hydra actions"
                  ("i" text-scale-increase "in")
                  ("o" text-scale-decrease "out")
                  ("v" shrink-window "shrink height")
                  ("^" enlarge-window "increase height")
                  ("<" shrink-window-horizontally "shrink width")
                  (">" enlarge-window-horizontally "shrink width")
                  ("n" winner-redo "Winner redo")
                  ("p" winner-undo "Winner undo")
                  ("q" nil "quit")))

(use-package himalaya)

(use-package man
  :custom
  (Man-width-max nil)
  (Man-switches "-a"))

(use-package tramp
  :custom
  (tramp-default-method "sshx")
  (tramp-use-connection-share nil))

(use-package bqn-mode
  :bind
  (:map bqn-mode-map
        ("C-M-x" . bqn-comint-eval-dwim))
  :config
  (progn
    (require 'smartparens)
    (sp-with-modes '(bqn-mode)
      (sp-local-pair "⟨" "⟩"))))

(use-package eat
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-term-scrollback-size (* 16 1024 1024))
  (eat-enable-yank-to-terminal t)
  (eat-line-input-ring-size 100000)
  (eat-line-input-history-isearch 'dwim))

(use-package ement
  :custom
  (ement-save-sessions t))

(use-package casual-calc
  :bind (:map calc-mode-map
              ("C-M-g" . casual-calc-tmenu)))

(use-package casual-image
  :bind (:map image-mode-map
              ("C-M-g" . casual-image-tmenu)))
