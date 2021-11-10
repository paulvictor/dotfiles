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

(use-package eshell
  ;;   :commands (eshell)
  :custom
  (eshell-history-file-name (no-littering-expand-var-file-name "eshell-history"))
  :config
  (setenv "PAGER" "cat") ; solves issues, such as with 'git log' and the default 'less'
  (setq
   eshell-history-size 4096
   eshell-hist-ignoredups t
   eshell-destroy-buffer-when-process-dies t)
  (with-eval-after-load 'em-term
    (dolist (p '("alsamixer" "htop" "mpv" "watch" "vim" "nvim" "rtorrent" "bluetoothctl"))
      (add-to-list 'eshell-visual-commands p))
    (setq eshell-visual-subcommands
          '(("git" "log" "diff" "show")
            ("sudo" "vi" "visudo"))))
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
           ("mkcd" "eshell/mkdir -p $* ; cd $1"))) ; TODO: '&&' does not work because mkdir exits with nil?
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
        ("C-k" . eshell-previous-matching-input-from-input)
        ("C-j" . eshell-next-matching-input-from-input)
        ("C-r" . pvr/esh-history)))

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
