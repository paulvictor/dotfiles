(in-package :stumpwm)

(defun find-window-by-name (name windows)
  (find-if (lambda (window)
             (search name (window-name window))) windows))

(defun find-window-globally-by-name (name &optional (group-list (screen-groups (current-screen))))
  (if (equal (car group-list) 'nil)
      'nil
      (let ((w (find-window-by-name name (list-windows (car group-list)))))
        (if w
            w ; stop if found and return window
            (find-window-globally-by-name name (cdr group-list))))))
