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

(defun pvr/esh-history ()
  "Interactive search eshell history."
  (interactive)
  (require 'em-hist)
  (save-excursion
    (let* ((start-pos (save-excursion (eshell-bol)))
            (end-pos (point-at-eol))
            (input (buffer-substring-no-properties start-pos end-pos))
            (command (ivy-read "Execute : "
                               (delete-dups
                                (when (> (ring-size eshell-history-ring) 0)
                                  (ring-elements eshell-history-ring)))
                               :initial-input input
                               :preselect 0
                               :require-match nil)))
      (kill-region start-pos end-pos)
      (insert command)))
  (end-of-line))

(defun pvr/eshell-quit-or-delete-char (arg)
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp))
      (progn
        (eshell-life-is-too-much)       ; Why not? (eshell/exit)
        (ignore-errors
          (delete-window)))
    (delete-forward-char arg)))

(use-package eshell
  :custom
  (eshell-history-file-name (no-littering-expand-var-file-name "eshell-history"))
  (eshell-prefer-lisp-functions t)
  (eshell-destroy-buffer-when-process-dies t)
  :config
  (setenv "PAGER" "cat") ; solves issues, such as with 'git log' and the default 'less'
  (setenv "GUIX_PROFILE" "/home/viktor/.config/guix/current")
  (add-to-list 'direnv-non-file-modes 'eshell-mode)
  (require 'em-pred)
;;   (require 'nix-shell)
  (setq
   eshell-history-size 4096
   eshell-hist-ignoredups t)
  (with-eval-after-load 'em-term
    (dolist (p '("alsamixer" "htop" "mpv" "watch" "vim" "nvim" "rtorrent" "bluetoothctl" "pscid" "ssh" "tail" "tmux" "screen" "nmtui" "ghci"))
      (add-to-list 'eshell-visual-commands p))
    (setq eshell-visual-subcommands
          '(("git" "log" "diff" "show")
            ("sudo" "vi" "visudo")
            ("sudo" "su")
            ("cabal" "repl")
            ("guix" "search"))))
  (with-eval-after-load 'em-alias
    (dolist
        (alias
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
           ("mkcd" "eshell/mkdir -p $* ; cd $1"); TODO: '&&' does not work because mkdir exits with nil?
           ("less" "view-file $1")))
      (add-to-list 'eshell-command-aliases-list alias)))
  ;;   (eshell-write-aliases-list)
  (setq eshell-input-filter
        (lambda (str)
          (not (or
                (string= "" str)
                (string= "cd" str)
                (string-prefix-p "cd " str)
                (string-prefix-p " " str)))))
  ;; (with-eval-after-load 'eshell-mode
  ;;     (general-define-key
  ;;      :keymaps 'eshell-mode-map
  ;;      :states '(insert normal emacs)
  ;;      "C-r" #'pvr/esh-history))
  :bind
  (:map eshell-mode-map
        ("C-d" . pvr/eshell-quit-or-delete-char)
        ("C-k" . eshell-previous-matching-input-from-input)
        ("C-j" . eshell-next-matching-input-from-input)
        ("C-r" . pvr/esh-history)
        ("C-a" . eshell-bol)))

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

(defun pvr/new-eshell ()
  (interactive)
  (let ((name (pvr/random-name)))
    (eshell "new")
    (rename-buffer (concat "*eshell " name " *"))))
