(in-package :stumpwm)

(ql:quickload "split-sequence")
(defun calc-screen-dimensions ()
  (mapcar (lambda (dim) (parse-integer dim))
          (split-sequence:SPLIT-SEQUENCE #\x
                                         (string-trim '(#\newline)
                                                      (run-shell-command "xrandr --current | grep '*' | uniq | awk '{print $1}'" t)))))

(defcommand invoke-equake () ()
  "Raise/lower Equake drop-down console."
  (let* ((on-top-windows (group-on-top-windows (current-group)))
         (equake-on-top (find-equake-in-group on-top-windows))
         (equake-width (car (calc-screen-dimensions)))
         (equake-height (round (* .40 (cadr (calc-screen-dimensions))))))
    (when (and equake-on-top (not (find-equake-globally (screen-groups (current-screen)))))
      (setf (group-on-top-windows (current-group)) (remove equake-on-top on-top-windows)))
    (if (and equake-on-top (eq (current-group) (window-group (find-equake-globally (screen-groups (current-screen))))))
        (progn (if (eq (find-class 'float-group) (class-of (current-group)))
                   (when (> (length (group-windows (current-group))) 1)
                     (xwin-hide equake-on-top))
                   (progn (unfloat-window equake-on-top (current-group))
                          (hide-window equake-on-top))) ;; then hide Equake window via native Stumpwm method.)
               (setf (group-on-top-windows (current-group)) (remove equake-on-top on-top-windows)))
        (let ((found-equake (find-equake-globally (screen-groups (current-screen))))) ; Otherwise, search all groups of current screen for Equake window:
          (if (not found-equake)          ; If Equake cannot be found,
              (progn (run-shell-command "emacsclient -n -e '(equake-invoke)'"))
              (progn (unless (eq (current-group) (window-group found-equake)) ; But if Equake window is found, and if it's in a different group
                       (move-window-to-group found-equake (current-group)))   ; move it to the current group,
                     (if (eq (find-class 'float-group) (class-of (current-group)))
                         (xwin-unhide (window-xwin found-equake) (window-parent found-equake))
                         (progn (unhide-window found-equake) ; unhide window, in case hidden
                                ;; (unfloat-window found-equake (current-group)) ;; in case in floating group
                                (raise-window found-equake)
                                (float-window found-equake (current-group)))) ; float window
                     (float-window-move-resize (find-equake-globally (screen-groups (current-screen))) :width equake-width :height equake-height) ; set size
                     (focus-window found-equake)
                     (push found-equake (group-on-top-windows (current-group))))))))) ; make on top

(defun find-equake-in-group (windows-list)
  "Search through WINDOWS-LIST, i.e. all windows of a group, for an Equake window. Sub-component of '#find-equake-globally."
  (let ((current-searched-window (car windows-list)))
    (if (equal current-searched-window 'nil)
        'nil
        (if (search "*EQUAKE*[" (window-name current-searched-window))
            current-searched-window
            (find-equake-in-group (cdr windows-list))))))

(defun find-equake-globally (group-list)
  "Recursively search through GROUP-LIST, a list of all groups on current screen, for an Equake window."
  (if (equal (car group-list) 'nil)
      'nil
      (let ((equake-window (find-equake-in-group (list-windows (car group-list)))))
        (if equake-window
            equake-window               ; stop if found and return window
            (find-equake-globally (cdr group-list))))))

(define-key *top-map* (kbd "F12") "invoke-equake")
