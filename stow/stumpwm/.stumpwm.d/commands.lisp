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

(ql:quickload "split-sequence")

(defcommand poison () ()
  (banish)
  (let* ((list-rats-cmd "xinput list | \grep -E 'slave\\s+pointer' | sed -E 's/([^=]*)=([0-9]+).*/\\2/'")
         (dev-ids (mapcar (lambda (dim) (parse-integer dim))
                          (split-sequence:SPLIT-SEQUENCE #\Newline
                                                         (string-trim '(#\Newline)
                                                                      (run-shell-command list-rats-cmd t))))))
    (dolist (rat-id dev-ids)
      (run-shell-command
       (concatenate 'string
                    "xinput "
                    (if *enabled?* "disable" "enable")
                    " '" (format nil "~a" rat-id) "'"))))
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

(defcommand eshell (name) ((:string "Name for the shell"))
  (run-shell-command "emacsclient -c -n -e '(pvr/new-eshell-window)'" t))
