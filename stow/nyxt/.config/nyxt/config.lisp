(in-package #:nyxt-user)

;; (define-configuration (buffer web-buffer)
;;   ((default-modes (append '(nyxt::emacs-mode) %slot-default%))))
;; (define-configuration (buffer web-buffer)
;;   ((default-modes (append '(nyxt::vi-normal-mode) %slot-default%))))

(start-swank)

(load "~/quicklisp/setup.lisp")

(ql:quickload :slynk)
(ql:quickload '("str" "cl-ppcre" "alexandria" "njson"))

(define-command-global start-slynk (&optional (slynk-port slynk::default-server-port))
  "Start a Slynk server that can be connected to, for instance, in Emacs via SLY."
  (slynk:create-server :port slynk-port :dont-close t)
  (echo "Slynk server started at port ~a" slynk-port))

(define-command-global delete-current-buffer ()
  "Delete current active buffer"
  (delete-buffer :buffers (list (current-buffer))))

(setf (uiop:getenv "GTK_THEME") "Adwaita:dark")

;; (define-configuration browser
;;     ((session-restore-prompt :always-restore)
;;      (autofills '((make-autofill :name "First Name" :fill "Paul Victor")
;;                   (make-autofill :name "Last Name" :fill "Raj")
;;                   (make-autofill :name "Name" :fill "Paul Victor Raj")
;;                   (make-autofill :name "Email" :fill "paulvictor@gmail.com")))
;;      (external-editor-program (or (uiop/os:getenv "EDITOR") "emacsclient -c"))))

(define-configuration browser
    ((external-editor-program (uiop:getenvp "EDITOR"))))

(defvar *search-engines-with-google-completions*
  (let ((ddg-completion
         (make-search-completion-function
          :base-url "https://duckduckgo.com/ac/?q=~a"
          ;; At some point try `curl "http://suggestqueries.google.com/complete/search?client=firefox&q=haskell"`
          :processing-function
          #'(lambda (results)
              (when results
                (map 'list (lambda (hash-table)
                             (first (alexandria:hash-table-values hash-table)))
                     (njson:decode results)))))
         ))
    (list  (make-instance 'search-engine
                          :name "DuckDuckGo"
                          :shortcut "ddg"
                          :search-url "https://duckduckgo.com/?q=~a"
                          :fallback-url (quri:uri "https://duckduckgo.com/")
                          :completion-function ddg-completion)
           (make-instance 'search-engine
                          :name "Haskell Docs"
                          :shortcut "hs"
                          :search-url "https://hoogle.haskell.org/?hoogle=~a"
                          :fallback-url (quri:uri "https://hoogle.haskell.org"))
           (make-instance 'search-engine
                          :name "Google"
                          :shortcut "g"
                          :search-url "https://google.com/search?q=~a"
                          :fallback-url (quri:uri "https://google.com")
                          :completion-function ddg-completion))))

(define-configuration context-buffer
    "Configure search engines manually"
  ((search-engines *search-engines-with-google-completions*)))

;; From https://discourse.atlas.engineer/t/change-keybinding/593
(defmacro alter-keyscheme (prefix keyscheme scheme-name &body bindings)
  #+nyxt-2
  `(let ((scheme ,keyscheme))
     (keymap:define-key (gethash ,scheme-name scheme)
       ,@bindings)
     scheme)
  #+nyxt-3
  `(keymaps:define-keyscheme-map ,prefix (list :import ,keyscheme)
     ,scheme-name
     (list ,@bindings)))

(define-configuration hint-mode
    "Customizing hint mode"
  ((visible-in-status-p nil)
   (hinting-type :vi)
   (hints-alphabet "neioarst")))

(define-configuration base-mode
    "Custom Rebind "
  ((keyscheme-map
    (alter-keyscheme "base-custom"
                     %slot-value%
                     nyxt/keyscheme:emacs
                     "C-_" 'reopen-last-buffer
                     "C-space" 'nyxt/mode/visual:toggle-mark
                     "C-x k" 'delete-current-buffer
                     "C-x C-k" 'delete-buffer
                     "C-:" 'nyxt/mode/visual:visual-mode))))

(define-configuration visual-mode
    "Configure visual mode"
  ((keyscheme-map (alter-keyscheme "visual-override"
                   %slot-value%
                   nyxt/keyscheme:emacs
                   "escape" 'visual-mode
                   "C-g" 'clear-selection))))

(define-configuration buffer
  ((default-modes
       (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))
   (override-map
    (let ((map (make-keymap "override-map")))
      (define-key map "M-x" 'execute-command)))))

;; (define-configuration input-buffer
;;   ((override-map
;;     (let ((map (make-keymap "override-map")))
;;       (define-key map "M-x" 'execute-command)))))
;; ;; (define-configuration nyxt/hint-mode:hint-mode
;; ;;   ((auto-follow-hints-p t)))

;; (define-configuration nyxt/web-mode:web-mode
;;   ((nyxt/web-mode:hints-alphabet "asdfghjkl")
;;    (keymap-scheme
;;     (define-scheme (:name-prefix "vi-ex" :import %slot-default%)
;;       scheme:vi-normal
;;       (list
;;  ;;        "M-:" 'eval-expression
;;        "r" 'reload-current-buffer
;;        "R" 'reload-buffers
;;        "J" 'switch-buffer-previous
;;        "K" 'switch-buffer-next
;;        "C-c y" 'nyxt/web-mode:copy-hint-url
;;        "C-c d" 'nyxt/web-mode:download-hint-url
;;        "x" 'delete-current-buffer
;;        "b" 'switch-buffer
;;        "t" 'pvr/open-new-tab
;;        "T" 'set-url-new-buffer
;;        "o" 'pvr/open-url
;;        "v" 'visual-mode
;;        "C-x k" 'delete-current-buffer)))))
