(in-package :stumpwm)

(setf *group-format* "%s [%n] %t ")
(setf *window-format* "%m%n%s%c")

(setf *window-border-style* :thin)
(setf *normal-border-width* 1)

(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

(setf *mouse-focus-policy* :click)
(set-fg-color "green")
