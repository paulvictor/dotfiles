{ pkgs }:

pkgs.writeText "init.el" ''
  (require 'package)

  ;; optional. makes unpure packages archives unavailable
  (setq package-archives nil)

  (setq package-enable-at-startup nil)
  (package-initialize)

  (setq w32-enable-italics t)    ; This must be done before font settings!
  ; (set-frame-parameter (selected-frame) 'alpha '(85 . 70))
  ; (add-to-list 'default-frame-alist '(alpha . (85 . 70)))
  (add-to-list 'default-frame-alist '(font . "VictorMono Nerd Font-12"))
  (load-theme 'doom-tomorrow-night t)
  (setq
    doom-themes-enable-bold t    ; if nil, bold is universally disabled
    doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (evil-collection-init)

  ; evil mode overrides
  (setq evil-want-C-u-scroll t)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-flash-delay 5)
  (setq evil-undo-system "undo-fu")
  (setq-default indent-tabs-mode nil)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;No menu bar
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;Show matching parens
  (show-paren-mode)
  ; Darkside
  (evil-mode 1)
  (doom-modeline-mode 1)
  (ivy-mode 1)
  ;; (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (which-key-mode)
  (add-hook
    'prog-mode-hook
    'display-line-numbers-mode)
  (linum-relative-mode)
  (projectile-mode 1)
  (key-chord-mode 1)
  (persp-mode)

  ;; Enable fancy-dabbrev previews everywhere:
  (global-fancy-dabbrev-mode)

  ;; Bind fancy-dabbrev-expand and fancy-dabbrev-backward to your keys of
  ;; choice, here "TAB" and "Shift+TAB":
  (global-set-key (kbd "<tab>") 'fancy-dabbrev-expand)
  (global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)

  ;; Let dabbrev searches ignore case and expansions preserve case:
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  ;; Max time delay between two key presses to be considered a key chord
  (setq key-chord-two-keys-delay 0.2) ; default 0.1

  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x '") 'counsel-recentf)
  (key-chord-define-global ",." 'find-file-in-project)
  (key-chord-define-global ",," 'persp-ivy-switch-buffer)
  (key-chord-define-global "``" 'aw-flip-window)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f1> b") 'counsel-descbinds)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-x /") 'swiper-isearch)
  (global-set-key (kbd "C-x *") 'swiper-thing-at-point)
  (global-set-key (kbd "C-x 8") 'swiper-all-thing-at-point)
  (global-set-key (kbd "M-o") 'ace-window)
  (global-unset-key (kbd "C-/"))
  (global-set-key (kbd "C-/") 'counsel-rg)
  (setq ivy-height 20)

  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  (setq savehist-file "~/git/.emacs.d/personal/emacs-history")
  (savehist-mode 1)

  (setq inhibit-startup-screen t)

  ;; Change "yes or no" to "y or n"
  (fset 'yes-or-no-p 'y-or-n-p)

  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)

  (setq visible-bell t)
  (setq auto-save-default nil)

  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-modal-icon t)

  (global-set-key (kbd "C-?") 'help-command)
  (global-set-key (kbd "M-?") 'mark-paragraph)
  ; Kill current buffer instead of prompting
  (global-set-key (kbd "C-x k") 'kill-this-buffer)

  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "SPC") 'evil-ex)
  (define-key evil-visual-state-map (kbd "SPC") 'evil-ex)
  (add-hook 'term-mode-hook 'turn-off-evil-mode)
  (add-hook 'inferior-ess-mode-hook 'turn-off-evil-mode)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  ; In term mode turn off all related to evil mode
  (evil-set-initial-state 'term-mode 'emacs)

  (define-key minibuffer-local-map (kbd "ESC") 'keyboard-escape-quit)
  (define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
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
  (defun goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line number input"
    (interactive)
    (unwind-protect
        (progn
          (linum-mode 1)
          (goto-line (read-number "Goto line: ")))
      (linum-mode -1)))

  (setq ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy)
        (read-file-name . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))

  (add-hook 'after-init-hook 'global-company-mode)

  (which-key-setup-side-window-right-bottom)
  ;; max width of which-key frame: number of columns (an integer)
  (setq which-key-frame-max-width 60)

  ;; Keep this as last as possible after all the minor modes
  (envrc-global-mode)

  ;; max height of which-key frame: number of lines (an integer)
  (setq which-key-frame-max-height 20)
  (setq explicit-shell-file-name "${pkgs.zsh}/bin/zsh")
  (setq term-prompt-regexp "^\*>")

  (setq ess-ask-for-ess-directory nil)
  (setq show-trailing-whitespace t)
  (setq evil-shift-width 2)

  ;;  (setq elscreen-prefix-key (kbd "C-."))
  ;;
  ;;  (elscreen-start)
  ;;  (elscreen-separate-buffer-list-mode)
  ;;  (define-key elscreen-map "h" 'elscreen-previous)
  ;;  (define-key elscreen-map "l" 'elscreen-next)
  ;;  (define-key elscreen-map "x" 'elscreen-kill)
  ;;  (define-key elscreen-map "r" 'elscreen-screen-nickname)

  (global-set-key (kbd "M-h") 'windmove-left)
  (global-set-key (kbd "M-j") 'windmove-down)
  (global-set-key (kbd "M-k") 'windmove-up)
  (global-set-key (kbd "M-l") 'windmove-right)

  (defun split-term-below ()
     "Split term below and switch to it"
     (interactive)
        (let ((w (split-window-below)))
          (select-window w)
          (vterm)
          (windmove-down)))
''
