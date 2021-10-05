(in-package :stumpwm)

; Modified from : https://searchcode.com/codesearch/view/2032904/
(defcommand pvr/gselect-or-create
    (group-number)
    ((:number "Group number: "))
  (let ((group-name (format nil "unnamed-~A"))
        (unnamed-groups-base 10))
    (gselect
     (or (select-group (current-screen) group-name)
         (let ((group (add-group (current-screen) group-name)))
           ;; number should be free, since select-group failed.
           (setf (group-number group) (unnamed-groups-base + group-number))
           group)))))

(defvar *enabled?* t)
(defvar *rat-name* "Logitech Wireless Mouse")

(defcommand poison () ()
  (banish)
  (run-shell-command
   (concatenate 'string
                "xinput "
                (if *enabled?* "disable" "enable")
                " '" *rat-name* "'"))
  (setf *enabled?* (not *enabled?*)))

(defun shell-command (command)
  "Run a shell command and display output to screen.
  This must be used in a functional side-effects-free style! If a program does not
  exit of its own accord, Stumpwm might hang!"
  (check-type command string)
  (echo-string (current-screen) (run-shell-command command t)))

(defcommand shell-exec (command) ((:string "sh:"))
  (shell-command command))

(defcommand toggle-float () ()
  (let
      ((w (current-window)))
    (if (eq 'TILE-WINDOW (type-of w))
        (float-window w (current-group))
        (unfloat-window w (current-group)))))
