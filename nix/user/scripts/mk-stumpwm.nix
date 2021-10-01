{pkgs ? import <nixpkgs> {}}:

pkgs.writeScript
  "init-stumpwm.lisp"
  ''
    #!${pkgs.sbcl}/bin/sbcl --script

    #-quicklisp
    (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                          (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
    (require :stumpwm)
    (stumpwm:stumpwm)
  ''
