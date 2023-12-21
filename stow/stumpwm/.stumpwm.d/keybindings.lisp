(in-package :stumpwm)

(defvar pvr/bindings (make-sparse-keymap))

;; (setf pvr/bindings (copy-kmap *root-map*))

(undefine-key *top-map* (kbd  "C-t"))

(set-prefix-key (kbd "s-g"))

(defun set-keybindings (key command &key (where 'both))
  (case where
    ('both
     (define-key *top-map* (kbd (concat "s-" key)) command)
     (define-key pvr/bindings (kbd key) command))
    ('top
     (define-key *top-map* (kbd (concat "s-" key)) command))
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
(set-keybindings "w" "delete-window" :where 'top)
;; (set-keybindings "Q" "quit-confirm" :where 'top)
(set-keybindings "o" "only" :where 'top)
(set-keybindings "f" "fullscreen" :where 'top)
(set-keybindings "F" "curframe" :where 'top)
(set-keybindings "\"" "frame-windowlist" :where 'top)
(set-keybindings "/" "windowlist" :where 'top)
(set-keybindings "i" "windowlist" :where 'top)
(set-keybindings "y" "hsplit" :where 'both)
(set-keybindings "z" "vsplit" :where 'both)
(set-keybindings "`" "gother" :where 'top)
(set-keybindings "n" "gother" :where 'top)
(set-keybindings "Tab" "gother" :where 'top)
; Setup so that hitting Caps lock twice does this
(set-keybindings "s-Up" "fother" :where 'local)
(set-keybindings "-" "vsplit" :where 'top)
(set-keybindings "\\" "hsplit" :where 'top)
(set-keybindings "|" "hsplit" :where 'top)

(set-keybindings "s-Left" "exchange-direction left" :where 'top)
(set-keybindings "s-Right" "exchange-direction right" :where 'top)
(set-keybindings "s-Up" "exchange-direction up" :where 'top)
(set-keybindings "s-Down" "exchange-direction down" :where 'top)

(define-key *top-map* (kbd "s-g") 'pvr/bindings)

(define-key *menu-map* (kbd "C-j") 'menu-down)
(define-key *menu-map* (kbd "C-k") 'menu-up)


(set-keybindings ";" "shell-exec" :where 'top)
(set-keybindings "SPC" "pull-hidden-next" :where 'top)
(set-keybindings "=" "poison" :where 'top)
(set-keybindings "RET" "eshell foo" :where 'top)
(set-keybindings "u" "remove-split" :where 'both)
(set-keybindings "r" "iresize" :where 'top)
