#!/usr/bin/env guile
!#

(let* ((path (dirname (current-filename)))
       (modules-path (string-join path "/guile-swayer")))
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
(load "keybindings.scm")
(keybindings-init)

;; subscribe to all events
(sway-subscribe-all)

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
  (hide-rofi-message)
  (display (format #f "notify-send -t 1000 -e \"~a\"" msg))
  (system (format #f "notify-send -t 1000 -e \"~a\"" msg)))

(define (hide-rofi-message)
  ;; (system "pkill -f '.*rofi -e.*'")
  (format #t "hiding the rofi message"))

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
  (hide-rofi-message))

;; add the display and hide hook functions
(add-hook! which-key-display-keybindings-hook show-which-key)
;; (add-hook! which-key-hide-keybindings-hook hide-which-key)

;; start listening to sway events
(sway-start-event-listener-thread)
(thread-join! SWAY-LISTENER-THREAD)
