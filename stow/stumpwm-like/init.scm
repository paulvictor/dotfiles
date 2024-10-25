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
             (ice-9 pretty-print)
             (libs sway-tree-helper)
             (swayipc))

;; (spawn-server (make-unix-domain-server-socket #:path "/tmp/viktor/swayer.sock"))
(spawn-server (make-tcp-server-socket #:host "0.0.0.0" #:port 12300))

(sway-connect-sockets!)

;; load look and feel
;; a separate scheme file for look and feel configuration
;; (load "behavior.scm")

;; init keybindings
;; a separate scheme file for keybindings (using general)
(load "keybindings.scm")
(keybindings-init)

;; subscribe to all events
(sway-subscribe-all)

;; (load "workspace-groups.scm")

;; configure workspace groups to sync groups
;; (define OUTPUTS '("eDP-1"))
;; (define GROUPS
;;   (map (lambda (l) (list (car l)))
;;        '(("11-browser" 		"21-browser" 	)
;;          ("12-development" 	"22-development")
;;          ("13-databases" 	"23-databases" 		)
;;          ("14-communication" "24-communication")
;;          ("15-development" 	"25-development" 	)
;;          ("16-gaming" 		"26-gaming" 		)
;;          ("17-mail" 			"27-mail" 			)
;;          ("18-development" 	"28-development")
;;          ("19-media" 		"29-media" 			))))

;; ;; (define OUTPUTS '("HDMI-A-1" "DP-1" ))
;; ;; (define GROUPS
;; ;;   '(("11-browser" 		"21-browser" 	)
;; ;;     ("12-development" 	"22-development")
;; ;;     ("13-databases" 	"23-databases" 		)
;; ;;     ("14-communication" "24-communication")
;; ;;     ("15-development" 	"25-development" 	)
;; ;;     ("16-gaming" 		"26-gaming" 		)
;; ;;     ("17-mail" 			"27-mail" 			)
;; ;;     ("18-development" 	"28-development")
;; ;;     ("19-media" 		"29-media" 			)))

;; (workspace-groups-configure #:groups GROUPS #:outputs OUTPUTS)
;; (workspace-groups-init)

;; configure workspace grid to arrange workspaces in a matrix
;; (define ROWS 3)
;; (define COLUMNS 3)
;; (define WORKSPACES (apply map list GROUPS))

;; (workspace-grid-configure #:rows ROWS #:columns COLUMNS #:workspaces WORKSPACES)
;; (workspace-grid-init)

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

(define prev-focused-window (sway-tree-node-focused))
(define inactive-opacity 0.5)
(define (make-inactive-windows-transparent window-event)
  (display "window changed \n")
  (set! e window-event)
  (when (equal? "focus" (sway-window-event-change window-event))
    (let* ((this-window (sway-window-event-container window-event))
           (this-window-id (sway-tree-id this-window))
           (prev-window-id (sway-tree-id prev-focused-window)))
      (when (not (equal? this-window-id prev-window-id)) ;; See https://github.com/swaywm/sway/issues/2859
        (let* ((command (format #f "[con_id=\"~a\"] opacity ~a" prev-window-id inactive-opacity)))
          (sway-dispatch-command command)
          (sway-opacity SWAY-OPACITY-SET 1)))
      (set! prev-focused-window this-window))))

(add-hook! sway-window-hook make-inactive-windows-transparent)
;; start listening to sway events
(sway-start-event-listener-thread)
(thread-join! SWAY-LISTENER-THREAD)
