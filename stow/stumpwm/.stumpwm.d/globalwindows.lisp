(in-package :stumpwm)

(load-module "globalwindows")

(set-keybindings "." "global-windowlist" :where 'top)
(set-keybindings "t" "global-windowlist" :where 'top)
(set-keybindings "," "global-pull-windowlist" :where 'top)
