(defun eshell-narrow-to-prompt ()
  "Narrow buffer to prompt at point."
  (interactive)

  (narrow-to-region
   (save-excursion
     (forward-line)
     (call-interactively #'eshell-previous-prompt)
     (beginning-of-line)
     (point))
   (save-excursion
     (forward-line)
     (call-interactively #'eshell-next-prompt)
     (re-search-backward eshell-prompt-regexp nil t)
     (point))))

(defun eshell-kill-save-output ()
  "Copy the command and output to the kill ring"
  (interactive)
  (let
      ((beg (save-excursion
              (call-interactively #'eshell-previous-prompt)
              (beginning-of-line)
              (point)))
       (end (save-excursion
              (call-interactively #'eshell-next-prompt)
              (point))))
    (kill-ring-save beg end)))

(defun pvr/eshell-quit-or-delete-char (arg)
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp))
      (eshell-life-is-too-much)
    (delete-forward-char arg)))

(use-package em-term
  :custom
  (eshell-visual-commands
   '("alsamixer" "htop" "mpv" "watch" "vim" "nvim" "rtorrent" "bluetoothctl" "pscid" "ssh" "tail" "tmux" "screen" "nmtui" "ghci"))
  (eshell-visual-subcommands
   '(("git" "log" "diff" "show")
            ("sudo" "vi" "visudo")
            ("sudo" "su")
            ("cabal" "repl")
            ("guix" "search")))
  :config
  (eat-eshell-mode))

(defun corfu-send-shell (&rest _)
  "Send completion candidate when inside comint/eshell."
  (cond
   ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
    (eshell-send-input))
   ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
    (comint-send-input))))

(defun eshell/less (&rest args)
  "Invoke `view-file' on a file (ARGS).
\"less +42 foo\" will go to line 42 in the buffer for foo."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (view-file file)
          (forward-line line))
      (view-file (pop args)))))

(defun eshell/quit-and-close (&rest _)
  "Quit the current eshell buffer and close the window it's in."
  (delete-frame))

;;;###autoload
(defun eshell/mcd (dir)
  "Create a directory (DIR) then cd into it."
  (make-directory dir t)
  (eshell/cd dir))
;;;###autoload
(use-package em-hist
  :custom
  (eshell-history-file-name (no-littering-expand-var-file-name "eshell/history"))
  (eshell-history-size 10000)
  (eshell-hist-ignoredups t)
  (eshell-input-filter
        (lambda (str)
          (not (or
                (string= "" str)
                (string= "cd" str)
                (string-prefix-p "cd " str)
                (string-prefix-p " " str)))))
  :bind
  (:map eshell-hist-mode-map
        ("M-r" . cape-history)))

(use-package eshell
  :custom
  (eshell-prefer-lisp-functions t)
  (eshell-destroy-buffer-when-process-dies t)
  :config
  (setenv "PAGER" "cat") ; solves issues, such as with 'git log' and the default 'less'
  (setq-mode-local eshell-mode
                   corfu-auto nil
                   corfu-quit-at-boundary t
                   corfu-quit-no-match t
                   completion-at-point-functions (list
                                                  (cape-capf-buster (cape-capf-super
                                                                     #'pcomplete-completions-at-point
                                                                     #'pcmpl-args-pcomplete-on-man
                                                                     #'cape-abbrev))
                                                  #'cape-file))

  (add-to-list 'direnv-non-file-modes 'eshell-mode)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
;;               (advice-add #'corfu-insert :after #'corfu-send-shell)
              (corfu-mode)))
  ;; (add-hook 'eshell-mode-hook
  ;;             (lambda ()
  ;;               (setq-local completion-at-point-functions
  ;;                           (append completion-at-point-functions
  ;;                                   (list pcomplete-completions-at-point #'cape-history #'cape-file)))))
  :bind
  (:map eshell-mode-map
        ("C-d" . pvr/eshell-quit-or-delete-char)
        ("C-a" . eshell-bol)))

(use-package em-pred)

(defvar pvr/eshell-aliases
         '(("l" "ls -1 $*")
           ("la" "ls -lAh $*")
           ("ll" "ls -lh $*")
           ("cpv" "cp -iv $*")
           ("mvv" "mv -iv $*")
           ("rmv" "rm -v $*")
           ("md" "eshell/mkdir -p $*")
           ("e" "find-file $1")
           ("ee" "find-file-other-window $1")
           ("clipcopy" "xclip -in -selection clipboard")
           ("clippaste" "xclip -out -selection clipboard")

           ("gd" "magit-diff-unstaged")
           ("gds" "magit-diff-staged")
           ("d" "dired-other-window $1")
           (".." "cd ..")

           ("quit" "quit-and-close")
           ("q"  "quit-and-close")
           ("f"  "find-file $1")
           ("ff" "find-file-other-window $1")
           ("d"  "dired $1")
           ("gg" "magit-status")
           ("proj" "project-switch-project")
           ("rg" "rg --color=always $*")

           ("clear" "clear-scrollback")))
(use-package em-alias
  :ensure nil
  :defer t
  :preface

  :config
  (advice-add #'eshell-write-aliases-list :override #'ignore)

  (defalias 'eshell/more #'eshell/less)

  (setq eshell-command-aliases-list
        (append eshell-command-aliases-list
                pvr/eshell-aliases)))

(use-package eshell-prompt-extras
  :demand t
  :config
  (setq eshell-prompt-function #'epe-theme-dakrone))

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

; From https://gitlab.com/ambrevar/dotfiles/-/blob/master/.emacs.d/lisp/init-eshell.el
;;; Shared history.
(defvar pvr/eshell-history-global-ring nil
  "The history ring shared across Eshell sessions.")

(defun pvr/eshell-hist-use-global-history ()
  "Make Eshell history shared across different sessions."
  (unless pvr/eshell-history-global-ring
    (when eshell-history-file-name
      (eshell-read-history nil t))
    (setq pvr/eshell-history-global-ring (or eshell-history-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring pvr/eshell-history-global-ring))
(add-hook 'eshell-mode-hook 'pvr/eshell-hist-use-global-history)

;; (defun pvr/eshell-history-remove-duplicates ()
;;   (require 'functions) ; For `pvr/ring-delete-first-item-duplicates'.
;;   (pvr/ring-delete-first-item-duplicates eshell-history-ring))
;; (add-hook 'eshell-pre-command-hook 'pvr/eshell-history-remove-duplicates)

;; Always save history
(add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

(use-package ansi-color
  :ensure nil
  :after eshell
  :init
  (add-hook 'eshell-preoutput-filter-functions
            ; Change to ansi-color-filter-apply if it's too slow
            'ansi-color-apply))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(bind-key "C-!" 'eshell-here)

;;;; Smarter EShell
;;;;

(use-package em-smart
  :ensure nil
  :defer t
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t)
  :config
  (eshell-smart-initialize))

(use-package casual-eshell
  :bind (:map eshell-mode-map
              ("C-M-g" . casual-eshell-tmenu)))
