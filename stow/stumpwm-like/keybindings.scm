(use-modules (modules kbd)
             (modules general)
             (swayipc)
             (ice-9 popen)
             (srfi srfi-18)
             (srfi srfi-13)
             (ice-9 textual-ports))

(define (exec command)
  "execute given shell command"
  (format #t "running: ~a\n" command)
  (thread-start! (make-thread (lambda () (system command)))))

;; get focused workspace from a list of workspaces
(define* (focused-output-name #:optional (workspaces (sway-get-workspaces)))
  (cond
    ((null? workspaces) #f)
    ((equal? #t (sway-workspace-focused (car workspaces)))
     (sway-workspace-output (car workspaces)))
    (else (focused-output-name (cdr workspaces)))))

(define (keybindings-init)
  (kbd-init)

  (general-configure #:keybinding-translator kbd-translate)
  (general-init)

  ;; define root keybindings
  (for-each
   (lambda (i)
     (let* ((keychord (+ i 9))
            (with-prefix (format #f "s-~a" keychord))
            (with-shifted-prefix (format #f "s-S-~a" keychord)))

       (general-define-key with-prefix
                           `(sway-switch-workspace-id ,i)
                           #:wk (format #f "Switch to workspace ~a" i))
       (general-define-key with-shifted-prefix
                           `(sway-move-container-to-workspace ,i)
                           #:wk (format #f "Move container to workspace ~a" i))))

   (iota 9 1 1))
  (general-define-keys

   ;; window and group management
   `("s-f" (sway-fullscreen SWAY-FULLSCREEN-TOGGLE) #:wk "Toggle Fullscreen")

   ;; move focus
   `("s-h" (sway-focus-container SWAY-DIRECTION-LEFT) #:wk "Focus Container Left")
   `("s-j" (sway-focus-container SWAY-DIRECTION-DOWN) #:wk "Focus Container Down")
   `("s-k" (sway-focus-container SWAY-DIRECTION-UP) #:wk "Focus Container Up")
   `("s-l" (sway-focus-container SWAY-DIRECTION-RIGHT) #:wk "Focus Container Right")
   `("s-u" (sway-focus-container SWAY-HIERARCHY-PARENT) #:wk "Focus parent")
   `("s-S-u" (sway-focus-container SWAY-HIERARCHY-CHILD) #:wk "Focus child container")

   ;; move containers
   `("s-S-h" (sway-move-container SWAY-DIRECTION-LEFT) #:wk "Move Container Left")
   `("s-S-j" (sway-move-container SWAY-DIRECTION-DOWN) #:wk "Move Container Down")
   `("s-S-k" (sway-move-container SWAY-DIRECTION-UP) #:wk "Move Container Up")
   `("s-S-l" (sway-move-container SWAY-DIRECTION-RIGHT) #:wk "Move Container Right")

   ;; Tab like cycling
   `("s-." (sway-focus-container-sibling SWAY-SIBLING-NEXT) #:wk "Cycle Tabs Next")
   `("s-," (sway-focus-container-sibling SWAY-SIBLING-PREV) #:wk "Cycle Tabs Previous")

   `("s-d" (exec "fuzzel") #:wk "Applications")
   `("s-S-d" (exec "rofiElectronAppsRunner") #:wk "Electron Apps")
   `("s-RET" (exec "emacsclient -c -n -e \"(pvr/new-eshell-window)\""))
   `("s-w" (sway-kill) #:wk "Kill Window")
   `("s-S-RET" (exec "alacritty") #:wk "Spawn Terminal")
   `("s-e" (exec "emacsclient -c -n") #:wk "Emacs client")
   `("s-o" (exec "warpd --normal") #:wk "Control the rat with the mind")
   `("s-p" (exec "passdo") #:wk "Type out the password")
   `("s-TAB"
     (sway-switch-workspace SWAY-WORKSPACE-BACK-AND-FORTH #:auto-back-and-forth #f)
     #:wk "Switch to previous workspace")
   `("s-b" (sway-split-container SWAY-SPLIT-HORIZONTAL) #:wk "Split container horizontal")
   `("s-v" (sway-split-container SWAY-SPLIT-VERTICAL) #:wk "Split container vertical")
   `("s-t" (sway-split-container SWAY-SPLIT-TOGGLE) #:wk "Split container toggle"))

  (general-define-keys
   #:prefix "s-DEL" #:wk "Exit"
   `("l" (exec "swaylock") #:wk "Lock")
   `("e" (exec "loginctl terminate-session \"\"") #:wk "Logout")
   `("s" (exec "systemctl suspend") #:wk "Suspend")
   `("r" (exec "systemctl reboot") #:wk "Reboot")
   `("x" (exec "systemctl poweroff") #:wk "Poweroff"))

  ;; define leader keymap
  (general-define-keys
   #:prefix "s-Space" #:wk "Leader"

   `(general-define-keys
     #:prefix "-" #:wk "Split"
     ("h" (sway-split-container SWAY-SPLIT-HORIZONTAL) #:wk "Split container horizontal")
     ("v" (sway-split-container SWAY-SPLIT-VERTICAL) #:wk "Split container vertical")
     ("t" (sway-split-container SWAY-SPLIT-TOGGLE) #:wk "Split container toggle")
     ("n" (sway-split-container SWAY-TOGGLE-NONE) #:wk "Split container none"))

   `(general-define-keys
     #:prefix "s" #:wk "Screenshot"
     ("g" (exec "slurp | grim -g - - | wl-copy") #:wk "Gui Screenshot")
     ("s" (exec (string-append "grim -o \"" (focused-output-name) "\" - | wl-copy")) #:wk "Current Screen")
     ("f" (exec "grim - | wl-copy") #:wk "All Screens")
     ("m" (exec "grim -g - - | wl-copy") #:wk "Last Region")

     (general-define-keys
      #:prefix "d" #:wk "DelayedScreenshot"
      ("g" (exec "sleep 2 && slurp | grim -g - - | wl-copy") #:wk "Gui Screenshot")
      ("s" (exec (string-append "sleep 2 && grim -o \"" (focused-output-name) "\" - | wl-copy")) #:wk "Current Screen")
      ("f" (exec "sleep 2 && grim - | wl-copy") #:wk "All Screens")
      ("m" (exec "sleep 2 && grim -g - - | wl-copy") #:wk "Last Region")))

   ;; session keymap
   `(general-define-keys
     #:prefix "DEL" #:wk "Session"
     ("q" (sway-exit) #:wk "Exit Sway")
     ("l" (exec "swaylock") #:wk "Lock session")
     ("r" (sway-reload) #:wk "Reload Sway"))

   `(general-define-keys
     #:prefix "w" #:wk "Window"
     ("v" (sway-layout SWAY-LAYOUT-SPLITV) #:wk "Split Vertically")
     ("h" (sway-layout SWAY-LAYOUT-SPLITH) #:wk "Split Horizontally")
     ("f" (sway-fullscreen SWAY-FULLSCREEN-TOGGLE) #:wk "Fullscreen")
     ("d" (sway-layout SWAY-LAYOUT-DEFAULT) #:wk "Default Layout")
     ("t" (sway-layout SWAY-LAYOUT-TABBED) #:wk "Tabbed Layout")
     ("SPC"
      (sway-layout-toggle
       (string-join '("stacking" "tabbed" "splitv" "splith") " ")) #:wk "Switch layouts"))))


   ;; screenshot keymap
   ;; flameshot is not performing well under wayland & multiple monitors
   ;; `(general-define-keys
   ;;   #:prefix "s" #:wk "Screenshot"
   ;;   ("d" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot gui"))
   ;;   ("s" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot screen"))
   ;;   ("f" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot full"))
   ;;   ("m" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot gui --last-region"))

   ;;   (general-define-keys
   ;;    #:prefix "d" #:wk "DelayScreenshot"
   ;;    ("d" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot gui -d 2500"))
   ;;    ("s" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot screen -d 2500"))
   ;;    ("f" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot full -d 2500"))
   ;;    ("l" (exec "export XDG_CURRENT_DESKTOP=sway && flameshot gui -d 2500 --last-region"))))


   ;; media-keys
;;    `("XF86AudioLowerVolume" (exec "pactl set-sink-volume @DEFAULT_SINK@ -5%") #:wk "Decrease Volume")
;;    `("XF86AudioRaiseVolume" (exec "pactl set-sink-volume @DEFAULT_SINK@ +5%") #:wk "Increase Volume")
;;    `("s-[" (exec "pactl set-sink-volume @DEFAULT_SINK@ -5%") #:wk "Decrease Volume")
;;    `("s-]" (exec "pactl set-sink-volume @DEFAULT_SINK@ +5%") #:wk "Increase Volume")
;;    `("XF86AudioMute" (exec "pactl set-sink-mute @DEFAULT_SINK@ toggle") #:wk "Toggle Mute")
;;    `("XF86AudioNext" (exec "mpc next") #:wk "Next Song")
;;    `("XF86AudioPrev" (exec "mpc prev") #:wk "Previous Song")
;;    `("XF86AudioPlay" (exec "mpc toggle") #:wk "Toggle Player")

;;    ;; brightness-keys
;;    `("XF86MonBrightnessUp" (exec "brightnessctl set +10%") #:wk "Increase Brightness")
;;    `("XF86MonBrightnessDown" (exec "brightnessctl set 10%-") #:wk "Decrease Brightness")

