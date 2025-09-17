;;;; Run commands in a popup frame
;; Using code from https://protesilaos.com/codelog/2024-09-19-emacs-command-popup-frame-emacsclient/

(defun pvr-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `pvr-window-popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'pvr-window-popup-frame)
    (delete-frame)))

(defmacro pvr-window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
Make the new frame have the `pvr-window-popup-frame' parameter."
  `(defun ,(intern (format "pvr-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `pvr-window-popup-frame' parameter.
Also see `pvr-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((pvr-window-popup-frame . t)
                                (window-system . x)
                                (display . ":0")))))
       (select-frame frame)
       (with-current-buffer (switch-to-buffer " pvr-window-hidden-buffer-for-popup-frame")
         (setq-local default-directory "~/") ; Sets the default directory to the home directory since it makes eshell open in a different directory otherwise
         (condition-case nil
             (call-interactively ',command)
           ((quit error user-error)
            (delete-frame frame)))))))

;; Dont know why it works even if this is commented out
;; (declare-function org-capture "org-capture" (&optional goto keys))
;; (defvar org-capture-after-finalize-hook)

;;;###autoload (autoload 'pvr-window-popup-org-capture "pvr-window")
(pvr-window-define-with-popup-frame org-roam-capture)

(add-hook 'org-capture-after-finalize-hook #'pvr-window-delete-popup-frame 50)

(defun pvr-eshell ()
  "Run eshell in a separate window"
  (interactive)
  (let ((name (pvr/random-name)))
    (eshell "new")
    (hide-mode-line-mode)
    (rename-buffer (concat "*EsHeLl: " name "*"))))
(pvr-window-define-with-popup-frame pvr-eshell)

(add-hook 'eshell-exit-hook #'pvr-window-delete-popup-frame 50)
