;;; init.el -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 20 1024 1024))
;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'package)
(require 'man)
(require 'ffap)

;; optional. makes unpure packages archives unavailable
(setq package-archives nil)

;; (setq enable-recursive-minibuffers t)

(setq package-enable-at-startup nil)
(package-initialize)

(defun set-env-vars ()
  (setenv "SSH_AUTH_SOCK"
          (substring
           (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket") 0 -1))
  (setenv "EDITOR" "emacsclient -c"))

(load-file (concat user-emacs-directory "names.el"))

(setq-default frame-title-format
              '(:eval
                (format "%s@%s: %s %s"
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        (or (file-remote-p default-directory 'host)
                            system-name)
                        (buffer-name)
                        (cond
                         (buffer-file-truename
                          (concat "(" buffer-file-truename ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t "[no file]")))))

(use-package f)

(defvar pvr/persist-dir
  (let ((d (or (getenv "PERSIST_DIR") "~/plain")))
    (unless (f-dir? d)
      (error "PERSIST_DIR not set"))
    d))

(defvar pvr/emacs-persist-dir
  (f-join pvr/persist-dir "emacs.d"))
(use-package s)
(use-package dash)

(use-package emacs
  :custom
  (tab-width 2)
  ;; Enable recursive minibuffers
  (enable-recursive-minibuffer t)
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
  (pulsar-global-mode 1)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (winner-mode 1)
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  ;; (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
  ;;         read-extended-command-predicate #'command-completion-default-include-p
  ;;         completions-detailed t)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  )

;; (use-package use-package-chords
;;   :init
;;   (setq key-chord-two-keys-delay 0.2) ; default 0.1
;;   :ensure t
;;   :config (key-chord-mode 1))

(use-package no-littering
  :init
  (setq no-littering-etc-directory
        (f-join pvr/emacs-persist-dir "emacs/")
        no-littering-var-directory
        (f-join pvr/emacs-persist-dir "var/" ))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(when
    (file-exists-p custom-file)
  (load-file custom-file))

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
    (local-set-key (kbd "q") 'kill-this-buffer)
    (current-buffer)))

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

;; (when (< (length command-line-args) 2)
;;   (add-hook 'emacs-startup-hook (lambda ()
;;                                   (when (display-graphic-p)
;;                                     (pvr/show-welcome-buffer)))))

(defun pvr/set-font-faces ()
  (set-mouse-color "white")
  (set-face-attribute 'default nil :family "VictorMono Nerd Font" :height 110 :weight 'bold)
  (set-face-attribute 'term nil :family "IosevkaTerm Nerd Font Mono" :height 60)
;;   (set-face-attribute 'fixed-pitch nil :font "Iosevka Fixed Slab" :height 110 :weight 'bold)
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

; if nil, italics is universally disabled
(setq w32-enable-italics t)
; This must be done before font settings!
; (set-frame-parameter (selected-frame) 'alpha '(85 . 70))
; (add-to-list 'default-frame-alist '(alpha . (85 . 70)))
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(scroll-bar-mode -1)
(setq visual-bell t)
(setq visible-bell t)
(setq whitespace-style '(trailing tabs))
(column-number-mode)
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook
  (lambda ()
    (whitespace-cleanup)))
(modify-all-frames-parameters '((fullscreen . maximized)))

(add-hook
  'prog-mode-hook
  (lambda ()
    (setq show-trailing-whitespace t)))

;Show matching parens
(show-paren-mode)

(use-package doom-themes
  :init
  (setq
   doom-themes-enable-bold t    ; if nil, bold is universally disabled
   doom-themes-enable-italic t)
  :config
  (load-theme 'doom-tomorrow-night t))

(use-package doom-modeline
  :custom
    (doom-modeline-window-width-limit fill-column)
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
    (doom-modeline-lsp t)
    (doom-modeline-modal-icon t)
  :config
  (doom-modeline-mode 1))

(use-package auth-source-pass
  :init
  (setq auth-sources '(password-store)
        auth-source-pass-filename "~/.password-store/non-yubikey")
)

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
(setq savehist-file (no-littering-expand-var-file-name "savehist") )
(savehist-mode 1)
(setq history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)
;; keymap-set and keymap-unset to bind keys in keymaps

;; Auto-revert to disk on file change
(global-auto-revert-mode t)

(defalias 'list-buffers 'ibuffer)

(use-package org-make-toc
    :hook (org-mode . org-make-toc-mode))

(use-package recentf
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 500))

(use-package pdf-tools
  :config
    (pdf-tools-install t t nil nil)
    (setq-default pdf-view-display-size 'fit-width)
    (add-hook 'pdf-view-mode-hook
      (lambda ()
        (set (make-local-variable 'evil-emacs-state-cursor) (list nil))
        (blink-cursor-mode -1)))
  :custom
    (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; enable eldoc for minibuffer evaluations use this snippet
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(use-package general
  :config
  (general-evil-setup t)
  (general-auto-unbind-keys)
;;   (general-define-key
;;     :states 'motion ; Normal, visual, operator states
;;     ";" 'evil-ex
;;     ":" 'evil-repeat-find-char)
  (general-create-definer pvr/space-keys-def
    :keymaps '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC"))

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

; Not explicitly included in emax/default.nix, but pulled in as a dependency of `slack`
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify))

(use-package slack
  :commands (slack-start)
  :init
    (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
    (setq slack-prefer-current-team t)
  :config
    (slack-register-team
      :name "juspay"
      :default t
      :token (auth-source-pick-first-password
                :host "juspay.slack.com"
                :user "paul.victor@juspay.in")
      :full-and-display-names t))

(setq auto-save-default nil)

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

(use-package evil
  :after (undo-tree)
  :custom
    (evil-vsplit-window-right t)
    (evil-split-window-below t)
    (evil-want-C-u-scroll t)
    (evil-disable-insert-state-bindings t)
    (evil-flash-delay 5)
    (evil-shift-width 2)
    (evil-undo-system 'undo-tree)
  :init
    (setq evil-want-integration t)
    (setq evil-search-module 'evil-search)
  :config
  (setq evil-want-keybinding nil)
  (evil-define-key '(insert visual) 'global (kbd "C-g") 'evil-normal-state)
  (evil-define-key 'normal 'global (kbd ", SPC") 'evil-ex-nohighlight)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1))

(use-package evil-collection
  :init
    (setq evil-want-keybinding nil)
  :config
    (setq evil-want-integration t)
    (evil-collection-init (remq 'lispy evil-collection-mode-list)))

;; (pvr/space-keys-def
;;   "SPC" 'lsp-keymap-prefix)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :after (evil-collection)
  :custom
  ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-x
  :ensure nil
  :bind
  (("C-x D" . dired-jump)))

(use-package dired-single
  :after (dired dired-jump)
  :bind
  ([remap dired-find-file] . dired-single-buffer)
  ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
  ([remap dired-up-directory] . dired-single-up-directory)
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(defun pvr/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(use-package ibuffer
  :hook
    (ibuffer-mode . hl-line-mode)
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

(global-set-key (kbd "C-x b") 'ibuffer)

; kill current buffer instead of prompting
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)

(add-hook 'term-mode-hook 'turn-off-evil-mode)
(setq explicit-shell-file-name "zsh")
(setq term-prompt-regexp "^\*>")

(use-package vterm :defer t)
(defun pvr/split-term ()
  "Split term below and switch to it"
  (interactive)
  (progn
    (split-window-sensibly (selected-window))
    (other-window 1)
    (projectile-run-vterm nil)))

(use-package org
  :hook
  (org-mode . (lambda ()
                (org-indent-mode)
                ;; (variable-pitch-mode 1)
                (auto-fill-mode 0)
;;                  Visual line mode messes up git gutter ;
;;                 (visual-line-mode 1)
                (setq evil-auto-indent nil)))
  :custom
  (org-directory "~/org-files")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)
     (shell . t)
     (lisp . t)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (font-lock-add-keywords
    'org-mode
    '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)
;;   (set-fringe-modeace-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 0.9)
                  (org-level-6 . 0.8)
                  (org-level-7 . 0.9)
                  (org-level-8 . 0.8)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))
  (setq org-capture-templates
        '(("l" "Useful Links" entry
           (file+olp "Links.org" "Links")
           "* %x :%^g")
          ("t" "Tasks" entry
           (file+olp "Tasks.org" "Tasks")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
          ("s" "Snippets" entry
           (file+olp "Snippets.org" "Snippets")
           "* %? \n ** %i\n"))))

(use-package org-tempo
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package org-indent)

(use-package org-superstar
  :custom
    (org-superstar-remove-leading-stars t)
    (org-superstar-headline-bullets-list
     '(
       ;;; Large
       "◉" "○" "●" "✸"
       ;;; Small
       "►" "•" "★" "▸"
       ))
  :init
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(use-package evil-org
  :after (evil evil-collection)
  :config
;;   (add-to-list 'evil-digit-bound-motions 'evil-org-beginning-of-line)
  (evil-define-key 'motion 'evil-org-mode
    (kbd "0") 'evil-org-beginning-of-line)
  :custom
  (evil-org-use-additional-insert t)
  :init
  (fset 'evil-redirect-digit-argument 'ignore)
  (add-hook 'org-mode-hook 'evil-org-mode))

(defun pvr/create-or-switch-perspective (dir)
  (persp-switch (f-filename dir)))

(use-package project
  :demand t
  :commands (project-switch-project)
  :custom
  (project-switch-use-entire-map t)
  :config
  (use-package perspective)
  (general-define-key
   :keymaps 'project-prefix-map
   :prefix "C-c"
   "e" 'project-eshell)
  (general-define-key
   :keymaps 'project-prefix-map
   "b" 'consult-project-buffer
   "/" 'consult-ripgrep
   "g" 'magit))

(use-package perspective
  :after project
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-initial-frame-name "Main")
  :config
  (advice-add 'project-switch-project :before #'pvr/create-or-switch-perspective)
  (persp-mode 1))

(use-package git-gutter
  :config
  (global-git-gutter-mode t))

(use-package magit
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-diff-refine-hunk t)
  :custom
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-z-for-folds t))
;;   :after (company company-prescient))

(evil-define-key '(motion insert) 'slime-repl-mode (kbd "C-c s") 'slime-selector)
  (evil-define-key '(motion insert) 'slime-mode (kbd "C-c s") 'slime-selector)

  (defun hyperspec-lookup--hyperspec-lookup-w3m (orig-fun &rest args)
    (let ((browse-url-browser-function 'w3m-browse-url))
      (apply orig-fun args)))

  ;; (use-package sly
;;     :hook (lisp-mode . sly-mode)
;;     :init
;;     (setq inferior-lisp-program "sbcl"))
                                        ; TODO : Move to dir specific config

  (use-package slime
    :hook (lisp-mode . slime-mode)
    :init
    (setq inferior-lisp-program "sbcl") ; TODO : Move to dir specific config
;;     (add-to-list 'slime-contribs 'slime-fancy)
    :config
    (advice-add 'hyperspec-lookup :around #'hyperspec-lookup--hyperspec-lookup-w3m)
    (add-hook 'slime-load-hook
              (lambda ()
                (define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup)))
    (require 'slime-autoloads))

  (use-package lispyville
    :hook
      ;; Check if we can use prog-mode ?
    ((slime-mode
      slime-repl-mode
      lisp-mode
      elisp-mode
      emacs-lisp-mode
      geiser-repl-mode
      ielm-mode
      scheme-mode) . lispyville-mode)
    :custom
      (lispyville-key-theme '(operators c-w c-u prettify additional-motions commentary slurp/barf-cp wrap additional additional-insert))
    :config
      (evil-define-key '(insert visual) lispyville-mode-map "(" 'lispy-parens
                                                            "[" 'lispy-brackets
                                                            ; Works with emacs lisp but org mode has some problem with the escaped quotes
                                                            (string #x22) 'lispy-quotes
                                                            "{" 'lispy-braces)
      ; Evil rebinds these on insert mode to normal mode switch and so am manually binding them
      (evil-define-key 'normal lispyville-mode-map "[" 'lispyville-next-opening
                                                    "{" 'lispyville-previous-opening
                                                    "]" 'lispyville-next-closing
                                                    "}" 'lispyville-previous-closing)
      (evil-define-key 'normal lispyville-mode-map (kbd "M-H") 'lispyville-beginning-of-next-defun))

(use-package ielm
  :bind
  ("C-l" . comint-clear-buffer)
  ("C-r" . consult-history)
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

(use-package anzu)

(use-package undo-tree
  :init
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(f-join pvr/emacs-persist-dir "undo"))))
  :config
  (global-undo-tree-mode 1))

;; (pvr/space-keys-def
;;   "l" '(:keymap lsp-command-map :wk "LSP"))

(pvr/space-keys-def
  :infix "h"
  ""  '(nil :wk "Help")
  "f" 'counsel-describe-function
  "p" 'helpful-at-point
  "b" 'counsel-descbinds
  "v" 'counsel-describe-variable
  "l" 'counsel-find-library)

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

(use-package origami
  :custom
  (origami-fold-replacement " ▾")
  :config
  (dolist (mode-hook '(emacs-lisp-mode-hook
                       nix-mode-hook
                       haskell-mode-hook
                       ess-r-mode-hook
                       eshell-mode-hook))
    (add-hook mode-hook
              #'(lambda ()
                  (origami-mode 1)))))

(pvr/space-keys-def
  :infix "z"
  ""  '(nil :wk "Fold")
  "o" 'origami-open-node
  "O" 'origami-open-node-recursively
  "c" 'origami-close-node
  "C" 'origami-close-node-recursively
  "t" 'origami-forward-toggle-node
  "TAB" 'origami-recursively-toggle-node
  "M-o" 'origami-open-all-nodes
  "M-c" 'origami-close-all-nodes
  "M-t" 'origami-toggle-all-nodes
  "j" 'origami-forward-fold-same-level
  "k" 'origami-backward-fold-same-level)

(use-package ess
  :custom
  (ess-use-company nil)
  :config
  (add-hook 'inferior-ess-mode-hook 'turn-off-evil-mode)

  (add-hook 'ess-r-help-mode
            #'(lambda () (evil-mode 1)))

  (setq ess-ask-for-ess-directory nil)

  (add-hook 'inferior-ess-r-mode-hook
            (lambda ()
              (local-set-key (kbd "C-j") 'comint-next-input)
              (local-set-key (kbd "C-k") 'comint-previous-input)))

  (setq display-buffer-alist
        `(("^\\*R Dired"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . right)
           (slot . -1)
           (window-width . 0.33)
           (reusable-frames . nil))
          ("^\\*R"
           (display-buffer-reuse-window display-buffer-at-bottom)
           (window-width . 0.5)
           (reusable-frames . nil))
          ("^\\*help[R]"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . right)
           (slot . 1)
           (window-width . 0.33)
           (reusable-frames . nil)))))

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

(evil-define-key '(visual normal insert) 'global (kbd "M-;") 'comment-dwim-line)

(use-package guru-mode
  :hook prog-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
  :after counsel
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package expand-region
  :config
  (evil-define-key '(normal emacs) 'global (kbd "C-'") 'er/expand-region)
  (evil-define-key '(normal emacs) 'global (kbd "C-\"") 'er/contract-region)
  (set-variable 'expand-region-subword-enabled t))

(pvr/space-keys-def
  "e" 'pvr/expand/body)

(use-package engine-mode
  :defer t
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "C-c s"))
  (defengine google "https://google.com/?q=%s"
    :keybinding "s")
  (defengine duckduckgo "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine hoogle "https://www.haskell.org/hoogle/?hoogle=%s"
    :keybinding "h"))

(setq home-row-keys
      (string-to-list "neio"))

(use-package avy
  :config
  (general-define-key
   :keymaps 'global
   :states '(normal insert emacs)
   "C-." 'avy-goto-char-timer
   "C-;" 'avy-goto-line
   "C-," 'avy-goto-line)
  (setq avy-keys home-row-keys)
  (setq avy-styles-alist
        '((avy-goto-char-2 . post)
          (avy-goto-line . pre)
          (avy-goto-char-timer . at-full))))

(use-package ace-window
  :init
  (setq aw-keys home-row-keys)
  :config
  (key-chord-define-global "``" 'aw-flip-window))

(use-package hydra)

(defhydra pvr/expand (:timeout 4)
  "Expand/Contract windows"
  ("h" er/contract-region "Contract")
  ("l" er/expand-region "Expand")
  ("q" nil "Quit" :exit t))

(defhydra pvr/window-ops (:timeout 4)
  "Move to windows"
  ("w" (ace-window))
  ("h" (windmove-left) "Left")
  ("j" (windmove-down) "Down")
  ("k" (windmove-up) "Up")
  ("l" (windmove-right) "Right")
  ("p" (evil-window-prev) "Previous")
  ("n" (evil-window-next) "Next")
  ("+" (evil-window-increase-height 1) "Increase height")
  ("-" (evil-window-decrease-height 1) "Decrease height")
  ("<" (evil-window-decrease-width 1) "Decrease width")
  (">" (evil-window-increase-width 1) "Increase width")
  ("=" (balance-windows) "Increase width")
  ("q" nil "Quit" :exit t))

(use-package zoom-window
  :custom
  (zoom-window-use-persp nil)
  (zoom-window-mode-line-color "Blue"))

;; Messes up git gutter
;; (use-package visual-fill-column
;;   :custom
;;   (fill-column 100)
;;   :defer t
;;   :hook
;;   (prog-mode . (lambda ()
;;                  (visual-line-mode 1)
;;                  (visual-fill-column-mode 1)))
;;   (org-mode . (lambda ()
;;                 (setq visual-fill-column-width 110
;;                       visual-fill-column-center-text t)
;;                 (visual-fill-column-mode 1))))

(define-key minibuffer-local-map (kbd "ESC") 'keyboard-escape-quit)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun insert-after-lispy-right (&optional count)
  (interactive "p")
  (let
      ((regex (s-concat "\s*" lispy-right)))
    (when (looking-at regex)
      (re-search-forward regex nil nil (or count 1)))))
(defun insert-before-lispy-left (&optional count)
  (interactive "p")
  (let
      ((regex (s-concat "\s*" lispy-left)))
    (when (looking-back regex)
      (re-search-backward regex nil nil (or count 1)))))
(evil-define-key '(insert) 'global
  (kbd "C-h") 'insert-before-lispy-left
  (kbd "C-l") 'insert-after-lispy-right)

(pvr/space-keys-def
  :infix ";"
  ""  '(nil :wk "Avy Goto")
  ";" 'avy-goto-char-timer
  "'" 'avy-goto-char-2
  "w" 'avy-goto-word-0
  "W" 'avy-goto-word-1
  "l" 'avy-goto-line)

(pvr/space-keys-def
  :infix "w"
  ""  '(nil :wk "Windows")
  "p" 'evil-window-prev
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right
  "n" 'evil-window-next
  "w" 'ace-window
  "+" 'evil-window-increase-height
  "-" 'evil-window-decrease-height
  "<" 'evil-window-decrease-width
  ">" 'evil-window-increase-width
  "x" 'evil-window-delete
  "s" 'ace-swap-window
  ";" 'pvr/window-ops/body
  "TAB" 'aw-flip-window
  "z" 'zoom-window-zoom
  "u" 'winner-undo
  "RET" 'split-window-horizontally
  "M-RET" 'split-window-vertically)

;; Risky, but I'm going to set it
(setq enable-local-eval t)

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

(load-file (concat user-emacs-directory "eshell.el"))

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

(load-file (concat user-emacs-directory "ghcid.el"))
(load-file (concat user-emacs-directory "utils.el"))

(use-package org-roam
  :custom
  (org-roam-directory "~/plain/roam-notes"))

(use-package hl-todo
  :init
  (global-hl-todo-mode)
  :custom
  hl-todo-keyword-faces '(("TODO" . "#FF0000")
                          ("FIXME" . "#FF0000")
                          ("DEBUG" . "#A020F0")
                          ("GOTCHA" . "#FF4500")
                          ("STUB" . "#1E90FF")))

(use-package pulsar
  :custom
  ((pulsar-pulse t)
   (pulsar-delay 0.05)
   (pulsar-iterations 10)
   (pulsar-face 'pulsar-magenta)
   (pulsar-highlight-face 'pulsar-yellow)

   (pulsar-pulse-functions
    '(isearch-repeat-forward
      isearch-repeat-backward
      evil-ex-search-next
      evil-ex-search-previous
      evil-ex-search-forward
      evil-ex-search-backward
      evil-avy-goto-line
      evil-avy-goto-char
      evil-avy-goto-char-timer
      evil-avy-goto-word-0
      evil-avy-goto-word-1
      recenter-top-bottom
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
      evil-scroll-up
      evil-scroll-down
      evil-scroll-page-up
      evil-scroll-page-down
      eshell-previous-prompt
      eshell-next-prompt)))
  :bind
  (("C-c C-x" . pulsar-pulse-line)))

(use-package edit-server
  :commands edit-server-start
  :init (if after-init-time
            (edit-server-start)
          (add-hook 'after-init-hook
                    #'(lambda () (edit-server-start))))
  :config (setq edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . nil)
                  (window-system . x))))

(defun pvr/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(pvr/space-keys-def
  "SPC" 'pvr/switch-to-last-buffer)

(pvr/space-keys-def
  "ESC" 'switch-to-buffer)

;; (use-package magit-delta
;;   :hook (magit-mode . magit-delta-mode))

(load-file (concat user-emacs-directory "completions.el"))

(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package electric
  :config
  (electric-indent-mode 1))

(defun show-C-h-prompt ()
  (interactive)
  (message "Use C-h"))

(global-set-key
 [f1] #'show-C-h-prompt)


;; Assuming the Guix checkout is in ~/guix.
(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/guix")
  (add-to-list 'geiser-guile-load-path "~/dotfiles/guix"))
(use-package geiser
  :config
  ;; (setq geiser-default-implementation 'gambit)
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile)))

(use-package password-store)

(use-package request)

(defun eh (query)
  (interactive "s\nQuery")
  (let* ((api-key
          ;; (s-trim-right (password-store--run "show" "api.openapi.com/key"))
          "foobar"
          )
;;         (url "https://api.openai.com/v1/chat/completions")
        (url "https://postman-echo.com/post")
        (output nil))
    (request url
      :type "POST"
      :headers '(("Content-Type" . "application/json")
                 ("Authorization" . (concat "Bearer " api-key)))
      :data (json-encode `(("model" . "gpt-3.5-turbo")
                           ("messages" . ,(vector '(("role" . "user")
                                                  ("content" . query))))
                           ("temperature" . 0.8)))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message "done %s" (assoc-default 'json data)))))))

(use-package webkit)

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))

