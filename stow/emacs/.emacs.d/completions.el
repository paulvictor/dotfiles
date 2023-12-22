(general-define-key
 :keymaps 'minibuffer-mode-map
 "C-r" #'minibuffer-complete-history)

(setq completion-auto-wrap t
;;       completion-auto-help 'always
      completion-show-help nil
      completions-format 'one-column
;;       completion-auto-select t
      completion-auto-select 'second-tab
      completions-max-height 10)

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
  :bind (:map minibuffer-mode-map
              ("<tab>" . minibuffer-next-completion)
              ("S-<tab>" . minibuffer-previous-completion))
  )


(use-package vertico
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ("M-RET" . minibuffer-force-complete-and-exit)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  (:map minibuffer-local-map
;;         ("<backspace>" . pvr/minibuffer-backward-kill)
        ("C-w" . backward-kill-word))
  :custom
  (vertico-count 12)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (setq history-length 100)
  (savehist-mode))

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
   ((string-prefix-p "RX" pattern) `(orderless-regexp . ,(substring pattern 2)))
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

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; TODO : Add a keybinding to consult-ripgrep
;; consult-buffer instead counsel-switch-buffer
;; consult-project-root-function

;; Try embark
(use-package consult
  :bind
  (("C-M-j" . persp-switch-to-buffer)
   :map minibuffer-local-map
        ("C-r" . consult-history))
  :general
  (:states 'normal
           "C-M-l"  #'consult-imenu
           "C-s" #'consult-line
           "C-x C-b" #'consult-buffer
           "C-y" #'consult-yank-from-kill-ring))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package corfu
  :init
  (global-corfu-mode)
  :hook
  (emacs-lisp-mode . corfu-mode)

  :custom
  (corfu-cycle t)
  (corfu-on-exact-match 'insert)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-preselect-first t)
  (corfu-preview-current 'insert)

  :general
  (:keymaps 'corfu-map
            :states 'insert
            "C-j" #'corfu-next
            "C-k" #'corfu-previous
            "C-n" #'corfu-next
            "C-p" #'corfu-previous
            "<escape>" #'corfu-quit
            "<tab>" #'corfu-insert
            "<return>" #'corfu-insert)
  :config
  (add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)))
  (evil-collection-corfu-setup)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (setq tab-always-indent 'complete
        completion-cycle-threshold 3
        read-file-name-completion-ignore-case t))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'
  )

(use-package pcmpl-args)

;; (use-package company
;;   :custom
;;     (company-dabbrev-other-buffers 'all)
;;     (company-dabbrev-time-limit 0.2)
;;     (company-dabbrev-code-time-limit 0.2)
;;     (company-dabbrev-downcase nil)
;;     (company-minimum-prefix-length 1))

(general-create-definer completions-pre
  :prefix "M-c")
(use-package cape
  :after (corfu)
  :config
  (require 'company-dabbrev)
  (require 'company-dabbrev-code)
  (require 'company-capf)
  (require 'company-files)

  (add-hook 'emacs-lisp-mode
            (lambda ()
              (make-local-variable 'completion-at-point-functions)

              (setf (elt (cl-member 'elisp-completion-at-point completion-at-point-functions) 0)
                    #'elisp-completion-at-point)
              (add-to-list 'completion-at-point-functions #'cape-symbol)
              ;; I prefer this being early/first in the list
              (add-to-list 'completion-at-point-functions #'cape-file)))
  (add-hook 'eshell-mode
            (lambda ()
              (setq-local completion-at-point-functions (append completion-at-point-functions
                                                                (list pcomplete-completions-at-point #'cape-history #'cape-file)))))

  (add-hook 'prog-mode-hook
            (lambda ()
              (make-local-variable 'completion-at-point-functions)
              (setq completion-at-point-functions
                    (list #'cape-keyword #'cape-dabbrev))
              (cl-loop for backend in (list #'company-dabbrev #'company-files #'company-dabbrev-code)
                       do
                       (add-to-list 'completion-at-point-functions
                                    (cape-company-to-capf backend)))
              ))
  ;; For pcomplete. For now these two advices are strongly recommended to
  ;; achieve a sane Eshell experience. See
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-shell-or-eshell

  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer and behaves as a pure
  ;; completion-at-point-function.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  :general
  (:prefix "M-c"
           "TAB" 'completion-at-point
           "k" 'cape-keyword
           "f" 'cape-file
           "s" 'cape-symbol
           "h" 'cape-history))

