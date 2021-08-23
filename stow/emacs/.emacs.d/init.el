;;; init.el -*- lexical-binding: t; -*-
(require 'package)

;; optional. makes unpure packages archives unavailable
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)
(setq gc-cons-threshold (* 4 1024 1024))

; if nil, italics is universally disabled
(setq w32-enable-italics t)
; This must be done before font settings!
; (set-frame-parameter (selected-frame) 'alpha '(85 . 70))
; (add-to-list 'default-frame-alist '(alpha . (85 . 70)))
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(font . "VictorMono Nerd Font-13")))
(set-mouse-color "white")
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(scroll-bar-mode -1)
(setq visual-bell t)
(setq auto-save-timeout nil)
(setq make-backup-files nil)
(setq tab-width 2)
(setq bookmark-save-flag 1)
(setq bookmark-use-annotations t)
(setq bookmark-size-search 100)
(setq whitespace-style '(trailing tabs))
(column-number-mode)
;; (global-display-line-numbers-mode t)
(setq inhibit-startup-screen t)
(setq comment-style "aligned")
;; TODO : Save it in a proper sync'able place
;; (setq savehist-file "~/git/.emacs.d/personal/emacs-history")
(savehist-mode 1)

(defun add-to-words-syntax (mode-hook chars)
  (seq-do
    #'(lambda (c)
       (add-hook mode-hook
        #'(lambda () (modify-syntax-entry ?- "w"))))
   chars))

(add-to-words-syntax 'emacs-lisp-mode-hook "-_")
(add-to-words-syntax 'nix-mode-hook "-_")
(add-to-words-syntax 'haskell-mode-hook "_")

;; enable paredit for minibuffer evaluations use this snippet
;; (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
;; enable eldoc for minibuffer evaluations use this snippet
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;Show matching parens
(show-paren-mode)

(use-package anzu)

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
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package no-littering
  :demand t
  :custom
  (no-littering-etc-directory
   (expand-file-name "config/" user-emacs-directory))
  (no-littering-var-directory
   (expand-file-name "data/" user-emacs-directory)))

(use-package use-package-chords
  :init
  (setq key-chord-two-keys-delay 0.2) ; default 0.1
  :ensure t
  :config (key-chord-mode 1))

(use-package git-gutter
  :config
    (global-git-gutter-mode t))

(use-package doom-themes
  :init
  (setq
    doom-themes-enable-bold t    ; if nil, bold is universally disabled
    doom-themes-enable-italic t)
  :config
  (load-theme 'doom-tomorrow-night t))

(use-package undo-tree
  :init
  :config
    (global-undo-tree-mode 1))

(use-package evil
  :after (undo-tree)
  :custom
    (evil-shift-width 2)
    (evil-vsplit-window-right t)
    (evil-split-window-below t)
    (evil-want-integration t)
    (evil-want-keybinding nil)
    (evil-want-C-u-scroll t)
    (evil-disable-insert-state-bindings t)
    (evil-flash-delay 5)
    (evil-shift-width 2)
    (evil-undo-system 'undo-tree)
  :init
    (setq evil-search-module 'evil-search)
  :config
    (evil-select-search-module 'evil-search-module 'evil-search)
    (evil-mode 1))

(use-package doom-modeline
  :custom
    (doom-modeline-window-width-limit fill-column)
    (doom-modeline-project-detection 'projectile)
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
    ;; (doom-modeline-lsp t)
    (doom-modeline-modal-icon t)
  :config
  (doom-modeline-mode 1))

(use-package ivy
  :config
    (ivy-mode 1)
  :custom
    (ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy)
        (read-file-name . ivy--regex-fuzzy)
        (swiper . ivy--regex-ignore-order)
        (counsel-M-x . ivy--regex-ignore-order)
        ;; (persp-ivy-switch-buffer . ivy--regex-fuzzy)
        ;; (find-file-in-project . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))
    (ivy-use-virtual-buffers t)
    (ivy-wrap t)
    (ivy-count-format "(%d/%d) ")
    (enable-recursive-minibuffers t)
    (ivy-initial-inputs-alist nil)
    (ivy-height 20)
  :bind
    (("C-x /" . swiper-isearch)
    ("C-x *" . swiper-thing-at-point)
    ("C-x 8" . swiper-all-thing-at-point)
      :map ivy-minibuffer-map
        ("<tab>" . ivy-alt-done)
        ("C-l" . ivy-alt-done)
        ("C-j" . ivy-next-line)
        ("C-k" . ivy-previous-line)
        ("C-<return>" . (lambda ()
                           (interactive)
                           (progn
                            (ivy-call)
                            (ivy-next-line))))
      :map ivy-switch-buffer-map
        ("C-k" . ivy-previous-line)
        ("C-j" . ivy-next-line)
        ("C-l" . ivy-done)
        ("C-d" . ivy-switch-buffer-kill)))

(use-package counsel
  :config
    (global-set-key [remap describe-function] 'counsel-describe-function)
    (global-set-key [remap describe-variable] 'counsel-describe-variable)
    (counsel-mode 1)
  :bind
    (("M-x" . counsel-M-x)
     ("C-x '" . counsel-recentf)
     ("M-y" . counsel-yank-pop)
     ("<f1> f" . counsel-describe-function)
     ("<f1> v" . counsel-describe-variable)
     ("<f1> l" . counsel-find-library)
     ("<f1> b" . counsel-descbinds)
     ("C-x C-f" . counsel-find-file)
     ("C-/" . counsel-rg)
     :map minibuffer-local-map
     ("C-r" . counsel-minibuffer-history)))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;; Implement a custom function for middle of the word completion like here :
;; https://github.com/company-mode/company-mode/issues/340
(use-package company
  :after (evil-collection)
  :demand t
  :config
    (unless (eq 'company-dabbrev (car company-backends))
      (push 'company-dabbrev company-backends))
    (push 'company-files company-backends)
    (company-prescient-mode 1)
    (company-tng-mode 1)
    (global-company-mode 1)
    ; Use tab key to cycle through suggestions.
    ; ('tng' means 'tab and go')
    (company-tng-configure-default)
  :init
    ;; Always have the ability to complete filenames
    (setq tab-always-indent 'complete)
    ; No delay in showing suggestions.
    (setq company-idle-delay 0)
  :custom
    (company-idle-delay 0)
    (company-selection-wrap-around t)
    (company-require-match nil)
    (company-dabbrev-other-buffers 'all)
    (company-dabbrev-time-limit 0.2)
    (company-dabbrev-code-time-limit 0.2)
    (company-dabbrev-downcase nil)
    (company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|_\\|-\\)")
    (company-minimum-prefix-length 3)
  :bind
    (:map company-active-map
      ("TAB" . company-complete-common-or-cycle)
      ("<backtab>" . company-select-previous)
      ("RET" . nil)
      ("C-j" . company-select-next-or-abort)
      ("C-k" . company-select-previous-or-abort)))

(use-package prescient
  :demand t
  :after (company)
  :commands prescient-persist-mode
  :init
    (setq prescient-history-length 30))

(use-package ivy-prescient
  :demand t
  :after (ivy counsel)
  :config
    (ivy-prescient-mode 1))

(use-package company-prescient
  :demand t
  :after (company prescient))

(use-package which-key
  :demand t
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

(use-package wgrep)

(use-package magit
  :after (company company-prescient))

(use-package origami)

(use-package ess
  :custom
  (ess-use-company nil)
  :config
  (add-hook 'inferior-ess-mode-hook 'turn-off-evil-mode)

  (add-hook 'ess-r-help-mode
            #'(lambda ()
                (evil-mode 1)))

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

(dolist (mode-hook '(emacs-lisp-mode-hook
                     nix-mode-hook
                     haskell-mode-hook
                     ess-r-mode-hook
                     shell-mode-hook
                     eshell-mode-hook))
  (add-hook mode-hook
    #'(lambda ()
        (origami-mode 1))))

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

(global-set-key (kbd "M-;") 'comment-dwim-line)

(add-hook 'before-save-hook
  (lambda ()
    (whitespace-cleanup)))

;; Sets up keybindings and stuff from default to ivy mode
(dolist (mode-hook '(org-mode-hook
                     vterm-mode-hook
                     term-mode-hook
                     shell-mode-hook
                     eshell-mode-hook))
  (add-hook mode-hook
    (lambda ()
      (progn
;;         (display-line-numbers-mode 0)
        (setq show-trailing-whitespace nil)))))

(use-package projectile
  :config
    (projectile-mode 1)
  :demand t
  :chords
    ((",." . projectile-find-file))
  :bind
    ("C-x C-r" . projectile-recentf)
  :init
    (when (file-directory-p "~/stuff")
      (setq projectile-project-search-path '("~/stuff"))))

(use-package counsel-projectile
  :after projectile
  :config
    (counsel-projectile-mode 1))

(use-package perspective
  :demand t
  :after (ivy projectile)
  :chords
    (",," . persp-ivy-switch-buffer)
  :custom
    (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

(use-package persp-projectile
  :demand t
  :after (perspective projectile counsel-projectile)
  :bind
    ([remap counsel-projectile-switch-project] . projectile-persp-switch-project))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line))

;; (setq-default show-trailing-whitespace nil)
;; Do we need the following code then ?
;; (add-hook
;;   'prog-mode-hook
;;   'display-line-numbers-mode)
(add-hook
  'prog-mode-hook
  '(lambda ()
    (setq show-trailing-whitespace t)))
(recentf-mode 1)
(add-hook
  'prog-mode-hook
  'rainbow-delimiters-mode)
(electric-indent-mode 1)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(defun init-dashboard ()
  (progn
    (switch-to-buffer "*dashboard*")
    (goto-char (point-min))
    (redisplay)))

(use-package dashboard
  :after projectile
  :config
    (dashboard-setup-startup-hook)
    (init-dashboard)
  :custom
    (dashboard-projects-backend 'projectile)
    (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
    (dashboard-startup-banner 'logo)
    (dashboard-set-heading-icons t)
    (dashboard-set-file-icons t)
    (dashboard-items '((recents . 5)
                      (bookmarks . 5)
                      (projects . 5))))

(use-package helpful
  :after counsel
  :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
  :bind
    ("<f1> p" . helpful-at-point)
    ([remap describe-symbol] . helpful-symbol)
    ([remap describe-command] . helpful-command)
    ([remap describe-key] . helpful-key))

(use-package expand-region
  :config
  (global-set-key (kbd "C-j") 'er/expand-region)
  (set-variable 'expand-region-subword-enabled t))

(use-package engine-mode
  :demand t
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "C-c s"))
  (defengine google "https://google.com/?q=%s"
    :keybinding "s")
  (defengine duckduckgo "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine hoogle "https://www.haskell.org/hoogle/?hoogle=%s"
    :keybinding "h"))

(use-package avy
  :config
    (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (setq avy-styles-alist
          '((avy-goto-char-2 . post)
            (avy-goto-line   . pre)
            (avy-goto-char-timer . at-full)))
  :bind
    ("C-;" . avy-goto-char-timer)
    ("C-:" . avy-goto-char-2)
    ("C-'" . avy-goto-line))

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define-global "``" 'aw-flip-window)

(use-package slime
  :init
    (setq inferior-lisp-program "sbcl") ; TODO : Move to dir specific config
  :config
    (add-hook 'slime-load-hook
      (lambda ()
        (define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup)))
    (require 'slime-autoloads))

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
  (ibuffer-old-time 24)
  )

(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "M-o") 'ace-window)

(use-package org
  :hook
  (org-mode . (lambda ()
                (org-indent-mode)
                ;; (variable-pitch-mode 1)
                (auto-fill-mode 0)
;;                  Visual line mode messes up git gutter ;
;;                 (visual-line-mode 1)
                (setq evil-auto-indent nil)))
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
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 0.9)
                  (org-level-6 . 0.8)
                  (org-level-7 . 0.9)
                  (org-level-8 . 0.8)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))
  (setq org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-refile-targets '(("Archive.org" :maxlevel . 1)))
  (setq org-capture-templates
    `(("t" "Tasks")
      ("tt" "Task" entry (file+olp "~/org-files/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
      ("n" "Notes")
      ("nn" "Notes" entry
           (file+olp+datetree "~/org-files/notes.org")
           "\n* %<%I:%M %p> - Notes : notes :\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)))
  (setq org-agenda-files
        '("~/org-files/tasks.org")))

(use-package org-tempo
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  )
(use-package org-indent)

(use-package org-superstar
  :after org
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

;;;;(use-package key-chord
;;;;  :after evil
;;;;  :init
;;;;    ;; Max time delay between two key presses to be considered a key chord
;;;;  :config)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))


(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(setq visible-bell t)
(setq auto-save-default nil)

(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)

; kill current buffer instead of prompting
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(define-key evil-normal-state-map (kbd "SPC") 'evil-ex)
(define-key evil-visual-state-map (kbd "SPC") 'evil-ex)
(add-hook 'term-mode-hook 'turn-off-evil-mode)
; In term mode turn off all related to evil mode

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-map (kbd "ESC") 'keyboard-escape-quit)
; (define-key swiper-map (kbd "<ESC>") 'minibuffer-keyboard-quit)

;  (add-hook 'some-mode-hook
;          (lambda ()
;            (define-key evil-normal-state-local-map
;                        (kbd "w") 'some-function)))

;  (defvar *my-linum-current-line-number* 0)
;
;  (setq linum-format 'my-linum-relative-line-numbers)
;
;  (defun my-linum-relative-line-numbers (line-number)
;    (let ((test2 (- line-number my-linum-current-line-number)))
;      (propertize
;      (number-to-string (cond ((<= test2 0) (* -1 test2))
;                              ((> test2 0) test2)))
;      'face 'linum)))
;
;  (defadvice linum-update (around my-linum-update)
;    (let ((my-linum-current-line-number (line-number-at-pos)))
;      ad-do-it))
;  (ad-activate 'linum-update)
;
;  (global-linum-mode t)
;  (linum-relative-on)

;(global-set-key [remap goto-line] 'goto-line-with-feedback)
;(defun goto-line-with-feedback ()
;  "Show line numbers temporarily, while prompting for the line number input"
;  (interactive)
;  (unwind-protect
;      (progn
;        (linum-mode 1)
;        (goto-line (read-number "Goto line: ")))
;    (linum-mode -1)))

(setq explicit-shell-file-name "zsh")
(setq term-prompt-regexp "^\*>")

(setq show-trailing-whitespace t)

(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-decrease)

(defun split-term-below ()
  "Split term below and switch to it"
  (interactive)
  (split-window-below)
  (projectile-run-vterm))
(global-set-key (kbd "C-x t") 'split-term-below)

(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

;; (setq lsp-keymap-prefix "C-c l")
;; (lsp-enable-which-key-integration t)

;; keep this as last as possible after all the minor modes
(envrc-global-mode)

(setf custom-file
      (let*
          ((init-file-components (s-split "/" (file-truename user-init-file)))
           (custom-file-components (-drop-last 1 init-file-components))
           (custom-file (s-join "/" (-snoc custom-file-components "custom.el"))))
        custom-file))
(when
  (file-exists-p custom-file)
  (load-file custom-file))

(setq enable-local-eval t)
