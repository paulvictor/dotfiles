(in-package #:nyxt-user)

;; (define-configuration (buffer web-buffer)
;;   ((default-modes (append '(nyxt::emacs-mode) %slot-default%))))
;; (define-configuration (buffer web-buffer)
;;   ((default-modes (append '(nyxt::vi-normal-mode) %slot-default%))))

(start-swank)

(load "~/quicklisp/setup.lisp")

(ql:quickload :slynk)

(define-command-global start-slynk (&optional (slynk-port slynk::default-server-port))
  "Start a Slynk server that can be connected to, for instance, in Emacs via SLY."
  (slynk:create-server :port slynk-port :dont-close t)
  (echo "Slynk server started at port ~a" slynk-port))

(define-command-global delete-current-buffer ()
  "Delete current active buffer"
  (delete-buffer :id (slot-value (current-buffer) 'id)))

(setf (uiop:getenv "GTK_THEME") "Adwaita:dark")

(define-configuration browser
    ((session-restore-prompt :always-restore)
     (autofills '((make-autofill :name "First Name" :fill "Paul Victor")
                  (make-autofill :name "Last Name" :fill "Raj")
                  (make-autofill :name "Name" :fill "Paul Victor Raj")
                  (make-autofill :name "Email" :fill "paulvictor@gmail.com")))
     (external-editor-program (or (uiop/os:getenv "EDITOR") "emacsclient -c"))))

(define-configuration buffer
  ((default-modes (append '(emacs-mode) %slot-default%))
   (override-map
    (let ((map (make-keymap "override-map")))
      (define-key map
        "J"   'switch-buffer-previous
        "K"   'switch-buffer-next
;;         "; y" 'nyxt/web-mode:copy-hint-url
;;         "; d" 'nyxt/web-mode:download-hint-url
        "x"   'delete-current-buffer
        "b"   'switch-buffer
        "M-x" 'execute-command)))))

(define-configuration web-buffer
  ((default-modes `(nyxt/emacs-mode:emacs-mode vi-normal-mode
                    ,@%slot-default%))))

(define-configuration prompt-buffer
  ((keymap-scheme-name scheme:emacs)
   (override-map
    (let ((map (make-keymap "prompt-map")))
      (define-key map
        "C-j" 'nyxt/prompt-buffer-mode:select-next
        "C-k" 'nyxt/prompt-buffer-mode:select-previous)))))

(define-command-global pvr/open-new-tab ()
  "Open a new tab, prompting for the URL"
  (set-url-new-buffer :prefill-current-url-p nil))

(define-command-global pvr/open-url ()
  "Open a new tab, prompting for the URL"
  (set-url :prefill-current-url-p nil))

;; (define-configuration nyxt/hint-mode:hint-mode
;;   ((auto-follow-hints-p t)))

(define-configuration nyxt/web-mode:web-mode
  ((nyxt/web-mode:hints-alphabet "asdfghjkl")
   (keymap-scheme
    (define-scheme (:name-prefix "vi-ex" :import %slot-default%)
      scheme:vi-normal
      (list
 ;;        "M-:" 'eval-expression
       "r" 'reload-current-buffer
       "R" 'reload-buffers
       "J" 'switch-buffer-previous
       "K" 'switch-buffer-next
       "C-c y" 'nyxt/web-mode:copy-hint-url
       "C-c d" 'nyxt/web-mode:download-hint-url
       "x" 'delete-current-buffer
       "b" 'switch-buffer
       "t" 'pvr/open-new-tab
       "T" 'set-url-new-buffer
       "o" 'pvr/open-url
       "v" 'visual-mode
       "C-x k" 'delete-current-buffer)))))
