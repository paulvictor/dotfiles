(in-package :stumpwm)

(defmacro if-sure (&body body)
  `(when (equal :yes (second (select-from-menu (current-screen)
                                               '(("no" :no) ("yes" :yes))
                                               "Sure?")))
     ,@body))

(defcommand session-menu () ()
  (let* ((menu '(("a. abort" :abort)
                 ("l. lock" :lock)
                 ("e. logout" :logout)
                 ("s. suspend" :suspend)
                 ("x. shutdown" :shutdown)
                 ("r. reboot" :reboot)
                 ("h. hibernate" :hibernate)))
         (selection (select-from-menu (current-screen) menu "Choose:"))
         (command (second selection)))
    (when command
      (case command
        (:abort (run-commands "echo Aborted."))
        (t (run-shell-command (format nil "wmexit ~(~a~)" command)))))))

(set-keybindings "DEL" "session-menu")
