#!/usr/bin/env guile
!#
; Assuming that this file has guile-swayers cloned in the same level
(let* ((path (dirname (current-filename)))
       (modules-path (string-join `(,path "guile-swayer") "/")))
  (format #t "adding ~a to load path \n" modules-path)
  (add-to-load-path modules-path))

(use-modules (oop goops)
             (srfi srfi-18)
             (modules workspace-groups)
             (modules workspace-grid)
             (modules auto-reload)
             (modules which-key)
             (system repl server)
             (swayipc))

;; (spawn-server (make-unix-domain-server-socket #:path "/tmp/viktor/swayer.sock"))
(spawn-server (make-tcp-server-socket #:host "0.0.0.0" #:port 12300))

(sway-connect-sockets!)

;; load look and feel
;; a separate scheme file for look and feel configuration
;; (load "behavior.scm")

;; init keybindings
;; a separate scheme file for keybindings (using general)
(load "workspace-groups.scm")
(load "keybindings.scm")
(keybindings-init)



;; subscribe to all events
(sway-subscribe-all)
(sway-switch-workspace-id 11)

;; https://willosborne.co.uk/coding/2019/11/16/iterators-and-for-loops-with-guile-scheme.html



;; configure auto reload to automatically reload sway when a config file changes
;; (auto-reload-configure #:directories
;;                        `(,(string-append (getenv "HOME") "/.config/sway/")))
;; (auto-reload-init)

;; configure which key to show available keybindings
(which-key-configure #:delay-idle 0.2)
(which-key-init)

(define (show-rofi-message msg)
  (let ((command  (format #f "notify-send -t 6000 -c sway.keymap -e \"~a\"" msg)))
    (display command)
    (system command)))

(define (show-which-key submap bindings)
  (format #t "Displaying Submap ~a Bindings:\n" submap)
  (let ((message ""))
    ;; printing to display (via repl/terminal)
    (for-each
     (lambda (ls)
       (let ((nmsg (format #f "    - ~a -> ~a\n" (list-ref ls 1) (list-ref ls 3))))
        (display nmsg)
        (set! message (string-append message nmsg))))
     bindings)

    ;; showing in rofi
    (show-rofi-message message)))

(define (hide-which-key submap)
  (format #t "Hiding Submap Bindings:\n")
  ;; hide your which-key viewer (rofi, eww, etc.)
  )

;; add the display and hide hook functions
(add-hook! which-key-display-keybindings-hook show-which-key)
;; (add-hook! which-key-hide-keybindings-hook hide-which-key)

;; start listening to sway events
(sway-start-event-listener-thread)
(thread-join! SWAY-LISTENER-THREAD)
