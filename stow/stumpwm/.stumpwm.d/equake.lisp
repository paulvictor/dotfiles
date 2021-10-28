(in-package :stumpwm)

(ql:quickload "split-sequence")
(defun calc-screen-dimensions ()
  (mapcar (lambda (dim) (parse-integer dim))
          (split-sequence:SPLIT-SEQUENCE #\x
                                         (string-trim '(#\newline)
                                                      (run-shell-command "xrandr --current | grep '*' | uniq | awk '{print $1}'" t)))))

;; (defcommand invoke-equake () ()
;;   "Raise/lower Equake drop-down console."
;;   (let* ((on-top-windows (group-on-top-windows (current-group)))
;;          (equake-on-top (find-equake-in-group on-top-windows))
;;          (equake-width (car (calc-screen-dimensions)))
;;          (equake-height (round (* .40 (cadr (calc-screen-dimensions))))))
;;     (when (and equake-on-top (not (find-equake-globally (screen-groups (current-screen)))))
;;       (setf (group-on-top-windows (current-group)) (remove equake-on-top on-top-windows)))
;;     (if (and equake-on-top (eq (current-group) (window-group (find-equake-globally (screen-groups (current-screen))))))
;;         (progn (if (eq (find-class 'float-group) (class-of (current-group)))
;;                    (when (> (length (group-windows (current-group))) 1)
;;                      (xwin-hide equake-on-top))
;;                    (progn (unfloat-window equake-on-top (current-group))
;;                           (hide-window equake-on-top))) ;; then hide Equake window via native Stumpwm method.)
;;                (setf (group-on-top-windows (current-group)) (remove equake-on-top on-top-windows)))
;;         (let ((found-equake (find-equake-globally (screen-groups (current-screen))))) ; Otherwise, search all groups of current screen for Equake window:
;;           (if (not found-equake)          ; If Equake cannot be found,
;;               (progn (run-shell-command "emacsclient -n -e '(equake-invoke)'"))
;;               (progn (unless (eq (current-group) (window-group found-equake)) ; But if Equake window is found, and if it's in a different group
;;                        (move-window-to-group found-equake (current-group)))   ; move it to the current group,
;;                      (if (eq (find-class 'float-group) (class-of (current-group)))
;;                          (xwin-unhide (window-xwin found-equake) (window-parent found-equake))
;;                          (progn (unhide-window found-equake) ; unhide window, in case hidden
;;                                 ;; (unfloat-window found-equake (current-group)) ;; in case in floating group
;;                                 (raise-window found-equake)
;;                                 (float-window found-equake (current-group)))) ; float window
;;                      (float-window-move-resize (find-equake-globally (screen-groups (current-screen))) :width equake-width :height equake-height) ; set size
;;                      (focus-window found-equake)
;;                      (push found-equake (group-on-top-windows (current-group))))))))) ; make on top

; Cases to handle
;; (if (no equake globally => run command)
;; (if (equake-on-top and equake is in current-group => hide equake))
;; (if (equake-on-top and equake is not in current-group => pull equake to current group, set it on top))
;; (if (not equake-on-top and equake is in current-group => set it on top in current window))
;; (if (not equake-on-top and equake is not in current-group => set it on top in current window))

(defcommand hide-this () ()
  (hide-window (current-window)))

(defcommand invoke-equake () ()
            "Raise/lower Equake drop-down console."
            (let ((equake-window (find-equake-globally))
                  (equake-width (car (calc-screen-dimensions)))
                  (equake-height (round (* 0.4 (cadr (calc-screen-dimensions))))))
              (if equake-window
                  (if (eq (window-group equake-window) (current-group))
                      (let ((on-top-w (group-on-top-windows (current-group))))
                        (if (eq (current-window) equake-window)
                            (progn
                              ;;                               (message-no-timeout "EQM : ~A" equake-window)
                              ;;                               (unfloat-window equake-window (current-group))
                              ;;                               (setf (group-on-top-windows (current-group)) (remove equake-window on-top-w))
                              ;;                               (break "foo")
                              (unfloat-window equake-window (current-group))
                              (hide-window equake-window)
                              (setf (group-on-top-windows (current-group)) (remove equake-window on-top-w))
                              (hide-window equake-window)
                              ;; (delete equake-window (group-on-top-windows (current-group)))
                              )
                            (progn
                              (unhide-window equake-window)
                              (raise-window equake-window)
;;                               (float-window equake-window (current-group))
;;                               (float-window-move-resize equake-window :width equake-width :height equake-height)
                              (setf (group-on-top-windows (current-group)) (adjoin equake-window on-top-w))
;;                               (push equake-window (group-on-top-windows (current-group)))
                              (focus-window equake-window))))
                      (progn
                        (move-window-to-group equake-window (current-group))
                        (unhide-window equake-window)
                        (float-window-move-resize equake-window :width equake-width :height equake-height)
                        (setf (group-on-top-windows (current-group)) (adjoin equake-window on-top-w))
;;                         (push equake-window (group-on-top-windows (current-group)))
                        (focus-window equake-window)))
                  (progn
                    (run-shell-command "emacsclient -n -e '(equake-invoke)'")
                    (bt:make-thread
                     (lambda ()
                       (let ((equake-window-found nil))
                         (loop while (not equake-window-found)
                            do
                              (progn
                                (setf equake-window-found (find-equake-in-group (group-windows (current-group))))
                                (sleep 0.1))))
                       (let ((equake-window (find-equake-in-group (group-windows (current-group)))))
                         (float-window equake-window (current-group))
                         (float-window-move-resize equake-window :width equake-width :height equake-height)
                         (focus-window equake-window)
                         (push equake-window (group-on-top-windows (current-group))))) :name "init-equake")))))

(defun find-equake-in-group (windows-list)
  "Search through WINDOWS-LIST, i.e. all windows of a group, for an Equake window. Sub-component of '#find-equake-globally."
  (find-if (lambda (window)
             (search "*EQUAKE*[" (window-name window))) windows-list))

(defun find-equake-globally (&optional (group-list (screen-groups (current-screen))))
  "Recursively search through GROUP-LIST, a list of all groups on current screen, for an Equake window."
  (if (equal (car group-list) 'nil)
      'nil
      (let ((equake-window (find-equake-in-group (list-windows (car group-list)))))
        (if equake-window
            equake-window               ; stop if found and return window
            (find-equake-globally (cdr group-list))))))

(define-key *top-map* (kbd "F12") "invoke-equake")
