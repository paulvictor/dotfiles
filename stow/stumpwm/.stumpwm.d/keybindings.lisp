(in-package :stumpwm)

(defvar pvr/bindings (make-sparse-keymap))

;; (setf pvr/bindings (copy-kmap *root-map*))

(defun set-keybindings (key command &key (where 'both))
  (case where
    ('both
     (define-key *top-map* (kbd (concat "C-M-s-" key)) command)
     (define-key pvr/bindings (kbd key) command))
    ('top
     (define-key *top-map* (kbd (concat "C-M-s-" key)) command))
    ('local
     (define-key pvr/bindings (kbd key) command))))

(loop for (vim-key name) in '(("k" "up")
                              ("j" "down")
                              ("h" "left")
                              ("l" "right"))
      do (let ((shifted-key (string-upcase vim-key)))
           (set-keybindings vim-key
                            (format nil "move-focus ~A" name) :where 'top)
           (set-keybindings shifted-key
                            (format nil "move-window ~A" name) :where 'top)))

(set-keybindings "?" '*help-map* :where 'top)
(set-keybindings "q" "delete-window" :where 'top)
(set-keybindings "Q" "quit-confirm" :where 'top)
(set-keybindings "o" "only" :where 'top)
(set-keybindings "f" "fullscreen" :where 'top)
(set-keybindings "F" "curframe" :where 'top)
(set-keybindings "\"" "frame-windowlist" :where 'top)
(set-keybindings "/" "windowlist" :where 'top)
(set-keybindings "`" "gother" :where 'top)
; Setup so that hitting Caps lock twice does this
(set-keybindings "C-M-s-Up" "fother" :where 'local)
(set-keybindings "-" "vsplit" :where 'top)
(set-keybindings "\\" "hsplit" :where 'top)

(set-keybindings "C-M-s-[" "exchange-direction left" :where 'top)
(set-keybindings "C-M-s-]" "exchange-direction right" :where 'top)
(set-keybindings "C-M-s-{" "exchange-direction up" :where 'top)
(set-keybindings "C-M-s-}" "exchange-direction down" :where 'top)

(define-key *top-map* (kbd "C-M-s-Up") 'pvr/bindings)

(define-key *menu-map* (kbd "C-j") 'menu-down)
(define-key *menu-map* (kbd "C-k") 'menu-up)


(set-keybindings ";" "shell-exec" :where 'top)
(set-keybindings "SPC" "toggle-float" :where 'top)
