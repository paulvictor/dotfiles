;;; ghcid.el --- Really basic ghcid+stack support in emacs with compilation-mode -*- lexical-binding: t -*-
;;; From https://github.com/ndmitchell/ghcid/blob/master/plugins/emacs/ghcid.el

;; Author: Matthew Wraith <wraithm@gmail.com>
;;         Yorick Sijsling
;; Maintainer: Matthew Wraith <wraithm@gmail.com>
;;             Yorick Sijsling
;;             Vasiliy Yorkin <vasiliy.yorkin@gmail.com>
;;             Neil Mitchell <ndmitchell@gmail.com>
;; URL: https://github.com/ndmitchell/ghcid
;; Version: 1.0
;; Created: 26 Sep 2014
;; Keywords: tools, files, Haskell

;;; Commentary:

;; Use M-x ghcid to launch

;;; Code:

(require 'term)

;; Set ghcid-target to change the stack target
(setq ghcid-target "")

(setq ghcid-process-name "ghcid")


(define-minor-mode ghcid-mode
  "A minor mode for ghcid terminals

Use `ghcid' to start a ghcid session in a new buffer. The process
will start in the directory of your current buffer.

It is based on `compilation-mode'. That means the errors and
warnings can be clicked and the `next-error'(\\[next-error]) and
`previous-error'(\\[previous-error]) commands will work as usual.

To configure where the new buffer should appear, customize your
`display-buffer-alist'. For instance like so:

    (add-to-list
     \\='display-buffer-alist
     \\='(\"*ghcid*\"
       (display-buffer-reuse-window   ;; First try to reuse an existing window
        display-buffer-at-bottom      ;; Then try a new window at the bottom
        display-buffer-pop-up-window) ;; Otherwise show a pop-up
       (window-height . 18)      ;; New window will be 18 lines
       ))

If the window that shows ghcid changes size, the process will not
recognize the new height until you manually restart it by calling
`ghcid' again.
"
  :lighter " Ghcid"
  (when (fboundp 'nlinum-mode) (nlinum-mode -1))
  (linum-mode -1)
  (compilation-minor-mode))


;; Compilation mode does some caching for markers in files, but it gets confused
;; because ghcid reloads the files in the same process. Here we parse the
;; 'Reloading...' message from ghcid and flush the cache for the mentioned
;; files. This approach is very similar to the 'omake' hacks included in
;; compilation mode.
(add-to-list
  'compilation-error-regexp-alist-alist
  '(ghcid-reloading
    "Reloading\\.\\.\\.\\(\\(\n  .+\\)*\\)" 1 nil nil nil nil
    (0 (progn
         (let* ((filenames (cdr (split-string (match-string 1) "\n  "))))
           (dolist (filename filenames)
             (compilation--flush-file-structure filename)))
         nil))
    ))
(add-to-list 'compilation-error-regexp-alist 'ghcid-reloading)


(defun ghcid-buffer-name (target)
  (concat "*" ghcid-process-name " " target "*"))

(defun ghcid-cmd (target)
  (format "cabal repl %s" target))

;; TODO Pass in compilation command like compilation-mode
(defun ghcid-command (h target)
    (format "ghcid -c \"%s\" -h %s\n" (ghcid-cmd target) h))

(defun ghcid-get-buffer (target)
  "Create or reuse a ghcid buffer with the configured name and
display it. Return the window that shows the buffer.

User configuration will influence where the buffer gets shown
exactly. See `ghcid-mode'."
  (display-buffer (get-buffer-create (ghcid-buffer-name target)) '((display-buffer-reuse-window))))

(defun ghcid-start (dir target)
  "Start ghcid in the specified directory"

  (with-selected-window (ghcid-get-buffer target)

    (setq next-error-last-buffer (current-buffer))
    (setq-local default-directory dir)

    ;; Only now we can figure out the height to pass along to the ghcid process
    (let ((height (- (window-body-size) 1)))

      (term-mode)
      (term-line-mode) ;; Allows easy navigation through the buffer
      (ghcid-mode)

      (setq-local term-buffer-maximum-size height)
      (setq-local scroll-up-aggressively 1)
      (setq-local show-trailing-whitespace nil)

      (term-exec (ghcid-buffer-name target)
                 (ghcid-buffer-name target)
                 "bash"
                 nil
                 (list "-c" (ghcid-command height target))))))

(defun ghcid-kill (target)
  (let* ((ghcid-buf (get-buffer (ghcid-buffer-name target)))
         (ghcid-proc (get-buffer-process ghcid-buf)))
    (when (processp ghcid-proc)
      (progn
        (set-process-query-on-exit-flag ghcid-proc nil)
        (kill-process ghcid-proc)
        ))))

;; TODO Close stuff if it fails
(defun ghcid (target)
  "Start a ghcid process in a new window. Kills any existing sessions.

The process will be started in the directory of the buffer where
you ran this command from."
  (interactive "sTarget to run : ")
  (ghcid-start default-directory target))

;; Assumes that only one window is open
(defun ghcid-stop (target)
  "Stop ghcid"
  (interactive "sTarget to stop : ")
  (ghcid-kill target)
  (kill-buffer (ghcid-buffer-name target)))

(provide 'ghcid)

;;; ghcid.el ends here
