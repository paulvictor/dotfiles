(in-package :stumpwm)

(setf *default-group-name* "Stuff")

; Source : https://github.com/alezost/stumpwm-config/blob/master/init.lisp
(defun pvr/load-module (name)
  "Load contributed stumpwm module NAME.
Return nil, if the module does not exist.
This function is similar to `load-module' command, except it returns nil
instead of any error."
  (let ((module (find-module (string-downcase name))))
    (and module
         (progn (asdf:operate 'asdf:load-op module)
                t))))

(defvar pvr/config-path
  (directory-namestring
   (truename (merge-pathnames
              (user-homedir-pathname)
              ".stumpwm.d")))
  "The root of all things stump")

(defun pvr/load (filename)
  (let ((file (merge-pathnames
               (concat filename ".lisp")
               pvr/config-path)))
    (if (probe-file file)
        (load file)
        (format *error-output* "File ~a doesn't exist." file))))

(redirect-all-output (merge-pathnames "log" pvr/config-path))

(let
    ((l/module-dir (uiop:subpathname* (user-homedir-pathname) ".stumpwm.d/modules")))
  (set-module-dir l/module-dir)
  (add-to-load-path l/module-dir))

(pvr/load "emacs")
(pvr/load "fonts")
(pvr/load "defaults")
(pvr/load "helpers")
(pvr/load "commands")
(pvr/load "keybindings")
(pvr/load "session")
(pvr/load "groups")
(pvr/load "globalwindows")
(pvr/load "border")
(pvr/load "modeline")
;; (pvr/load "equake")
; Message bar settings

(which-key-mode)
; Move to the first workspace
(ignore-errors (gselect "1"))
