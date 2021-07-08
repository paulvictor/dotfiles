;;; config.el -*- lexical-binding: t; -*-
(require 'package)

;; optional. makes unpure packages archives unavailable
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)

; if nil, italics is universally disabled
(setq w32-enable-italics t)
; This must be done before font settings!
; (set-frame-parameter (selected-frame) 'alpha '(85 . 70))
; (add-to-list 'default-frame-alist '(alpha . (85 . 70)))
(add-to-list 'default-frame-alist '(font . "VictorMono Nerd Font-12"))
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
(global-display-line-numbers-mode t)
(setq inhibit-startup-screen t)
(setq comment-style "aligned")
;; TODO : Save it in a proper sync'able place
;; (setq savehist-file "~/git/.emacs.d/personal/emacs-history")
(savehist-mode 1)

;; enable eldoc for minibuffer evaluations use this snippet
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; enable paredit for minibuffer evaluations use this snippet
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;Show matching parens
(show-paren-mode)

(use-package use-package-chords
  :init
  (setq key-chord-two-keys-delay 0.2) ; default 0.1
  :ensure t
  :config (key-chord-mode 1))

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
  :config
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
      :map ivy-switch-buffer-map
        ("C-k" . ivy-previous-line)
        ("C-j" . ivy-next-line)
        ("C-l" . ivy-done)
        ("C-d" . ivy-switch-buffer-kill)))

(use-package counsel
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

(use-package prescient
  :commands prescient-persist-mode
  :init
    (setq prescient-history-length 30))

(use-package ivy-prescient
  :after (ivy counsel)
  :config
    (ivy-prescient-mode 1))

(use-package company
  :after (prescient company-prescient)
  ;; :commands (global-company-mode company-prescient-mode company-tng-mode)
  :hook
    ((prog-mode . company-mode)
     (prog-mode . company-prescient-mode)
     (prog-mode . company-tng-mode))
  :config
    (unless (eq 'company-dabbrev (car company-backends))
      (push 'company-dabbrev company-backends))
  :init
    (setq tab-always-indent 'complete)
  :custom
    (company-idle-delay nil)
    (company-dabbrev-other-buffers t)
    (company-dabbrev-time-limit 0.2)
    (company-dabbrev-code-time-limit 0.2)
    (company-dabbrev-downcase nil)
    (company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|_\\|-\\)")
    (company-minimum-prefix-length 3)
  :bind
    (:map company-mode-map
      ("<tab>" . 'company-indent-or-complete-common)
    :map company-active-map
      ("C-n" . 'company-select-next-or-abort)
      ("C-j" . 'company-select-next-or-abort)
      ("C-p" . 'company-select-previous-or-abort)
      ("C-k" . 'company-select-previous-or-abort)))

(use-package company-prescient
  :after company)

(use-package which-key
  :custom
  (which-key-idle-delay 0.2)
  ;; max width of which-key frame: number of columns (an integer)
  (which-key-frame-max-width 60)
  ;; max height of which-key frame: number of lines (an integer)
  (which-key-frame-max-height 20)
 :config
 (which-key-setup-side-window-right-bottom)
 (which-key-mode 1))

(use-package perspective
  :demand t
  :custom
  (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

(use-package perspective
  :after (ivy projectile)
  :chords
    ((",," . persp-ivy-switch-buffer)))

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
(dolist (mode '(org-mode-hook
                vterm-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode
    (lambda ()
      (progn
        (display-line-numbers-mode 0)
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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line))

;; (setq-default show-trailing-whitespace nil)
;; Do we need the following code then ?
(add-hook
  'prog-mode-hook
  'display-line-numbers-mode)
(add-hook
  'prog-mode-hook
  '(lambda ()
    (setq show-trailing-whitespace t)))
(recentf-mode 1)
(add-hook
  'prog-mode-hook
  'rainbow-delimiters-mode)
(electric-indent-mode 1)

(setq dashboard-projects-backend 'projectile)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-startup-banner 'logo)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-items '((recents . 5)
                      (bookmarks . 5)
                      (projects . 5)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define-global "``" 'aw-flip-window)

(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "<f1> p") 'helpful-at-point)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key [remap describe-symbol] 'helpful-symbol)
(global-set-key [remap describe-function] 'counsel-describe-function)
(global-set-key [remap describe-variable] 'counsel-describe-variable)
(global-set-key [remap describe-command] 'helpful-command)
(global-set-key [remap describe-key] 'helpful-key)

;;;;(use-package key-chord
;;;;  :after evil
;;;;  :init
;;;;    ;; Max time delay between two key presses to be considered a key chord
;;;;  :config)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

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
(add-hook 'inferior-ess-mode-hook 'turn-off-evil-mode)
; In term mode turn off all related to evil mode

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-map (kbd "ESC") 'keyboard-escape-quit)
; (define-key swiper-map (kbd "<ESC>") 'minibuffer-keyboard-quit)

;(add-hook 'some-mode-hook
;          (lambda ()
;            (define-key evil-normal-state-local-map
;                        (kbd "w") 'some-function)))

;  (defvar my-linum-current-line-number 0)
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
; (linum-relative-on)

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

(setq ess-ask-for-ess-directory nil)
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

(dashboard-setup-startup-hook)

(defun init-dashboard ()
  (progn
    (switch-to-buffer "*dashboard*")
    (goto-char (point-min))
    (redisplay)))

(init-dashboard)

;; keep this as last as possible after all the minor modes
(envrc-global-mode)
