(in-package :stumpwm)

(setf *default-group-name* "Stuff")

(flet ((create-group (name number)
         (or
          (let ((g (select-group (current-screen) (format nil "~A" number))))
            (and g
                 (setf (group-name g) name)))
          (let ((g (add-group (current-screen) name)))
            (setf (group-number g) number)
            g))))
  (let* ((pvr/group-names
           '(Stuff Browse Communication Alpha Beta Gamma Docker Music .Misc))
         (pvr/group-numbers (alexandria:iota (length pvr/group-names) :start 1))
         (pvr/group-map (pairlis pvr/group-names pvr/group-numbers)))
    (dolist (pair pvr/group-map)
      (create-group (format nil "~(~a~)" (car pair)) (cdr pair))
      (set-keybindings (format nil "~a" (cdr pair)) (format nil "gselect ~a" (car pair)) :where 'top))))

(let ((shift-map '(("1" "!")
                   ("2" "@")
                   ("3" "#")
                   ("4" "$")
                   ("5" "%")
                   ("6" "^")
                   ("7" "&")
                   ("8" "*")
                   ("9" "("))))
  (loop for (n c) in shift-map
        do (set-keybindings (format nil "~a" c) (format nil "gmove ~a" n) :where 'top)))
