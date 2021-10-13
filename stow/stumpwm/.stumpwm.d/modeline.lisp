(in-package :stumpwm)

(ql:quickload "xembed")

(setf *mode-line-border-width* 3)
(setf *mode-line-timeout* 2)

(load-module "stumptray")
(load-module "cpu")
(load-module "net")
(load-module "wifi")
(load-module "mem")
(load-module "battery-portable")

(setf wifi:*iwconfig-path* "/run/current-system/sw/bin/iwconfig")

(defun pretty-time ()
  "Returns the date formatted as '17:19 Thu, 27 Oct."
  (flet ((dow-str (dow)
           (nth dow '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
         (mon-str (mon)
           (nth (- mon 1) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                            "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))))
    (multiple-value-bind (sec min hr date mon yr dow)
        (get-decoded-time)
      (format NIL "~2,'0d:~2,'0d | ~a, ~d ~a"
              hr min
              (dow-str dow)
              date (mon-str mon)))))

;; (setf cpu::*cpu-modeline-fmt* " %C ")
(setf mem::*mem-modeline-fmt* "%a %p %b")
(setf *screen-mode-line-format*
      (list "[^B%n^b] | %W" ; groups/windows
;;             (bar 20 10 #\| #\x)
            "^>" ; right align
            "| %C "
            "| %M "
            "| %l "
            "| %I "
            "| Bat: %B "
            "| ^7*" '(:eval (pretty-time)); date
            ))

(dolist (head (screen-heads (current-screen)))
  (unless (head-mode-line head)
      (toggle-mode-line (current-screen) head)))

;; No need for the tray for now
;; (setf stumptray::*tray-cursor-thickness* 4)
;; (setf stumptray::*tray-cursor-icon-distance* 4)

;; (stumptray:stumptray)
