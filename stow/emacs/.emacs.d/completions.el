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

(use-package vertico
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ("M-RET" . minibuffer-force-complete-and-exit)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  (:map minibuffer-local-map
        ("<backspace>" . pvr/minibuffer-backward-kill)
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
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles . (partial-completion)))
     (buffer (styles . (orderless basic)))))
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

(use-package pcmpl-args)

(use-package company
;;   :init
;;     (setq tab-always-indent 'complete)
;;     (add-hook 'prog-mode-hook #'pvr/setup-company)
;;     (add-hook 'org-mode-hook #'pvr/setup-company)
  :custom
;;     (company-idle-delay 0.0)
;;     (company-selection-wrap-around t)
;;     (company-require-match nil)
    (company-dabbrev-other-buffers 'all)
    (company-dabbrev-time-limit 0.2)
    (company-dabbrev-code-time-limit 0.2)
    (company-dabbrev-downcase nil)
    (company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|_\\|-\\)")
    (company-minimum-prefix-length 1)
;;   :bind
;;     (:map company-active-map
;;           ("TAB" . company-complete-common-or-cycle)
;;           ("<backtab>" . company-select-previous)
;;           ("RET" . company-complete-selection)
;;           ("C-j" . company-select-next-or-abort)
  ;;           ("C-k" . company-select-previous-or-abort))
  )

(use-package cape
  :after (corfu)
  :config
  (require 'company-dabbrev)
  (require 'company-dabbrev-code)
  (require 'company-capf)
  (require 'company-files)

;;   (nconc completion-at-point-functions
;;          (list #'cape-dabbrev #'cape-file))
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
              (cl-loop for backend in (list #'company-dabbrev #'company-files #'company-dabbrev-code)
                       do
                       (add-to-list 'completion-at-point-functions
                                    (cape-company-to-capf backend)))
              (setq completion-at-point-functions
                          (append completion-at-point-functions
                                  (list #'cape-keyword)))))
  ;; For pcomplete. For now these two advices are strongly recommended to
  ;; achieve a sane Eshell experience. See
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-shell-or-eshell

  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer and behaves as a pure
  ;; `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;; (pvr/add-company-backends)


;; Implement a custom function for middle of the word completion like here :
;; https://github.com/company-mode/company-mode/issues/340
;; (defun pvr/setup-company ()
;;   (company-mode 1)
;;   (company-prescient-mode 1)
;;   (company-tng-mode 1)
;;   (company-tng-configure-default))

;; (add-hook 'prog-mode-hook
;;   (lambda ()
;;     (setq company-backends
;;           '(company-dabbrev
;;             company-dabbrev-code
;;             company-files
;;             company-capf))))
