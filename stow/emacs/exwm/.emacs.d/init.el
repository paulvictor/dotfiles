(require 'package)
(require 'man)
(require 'ffap)
;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Give the frame basic coloring while waiting for the theme to load. The main
;; purpose of this is to not blind me when it's dark by flashing a screen full
;; of white. These colors are from doom-one.
;; (set-face-attribute 'default nil :background "#282c34" :foreground "#bbc2cf")
;; Default frame settings. This is actually maximized, not full screen.
(push '(fullscreen . maximized) initial-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
; if nil, italics is universally disabled
(setq w32-enable-italics t)
; This must be done before font settings!
; (set-frame-parameter (selected-frame) 'alpha '(85 . 70))
; (add-to-list 'default-frame-alist '(alpha . (85 . 70)))
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(scroll-bar-mode -1)

(modify-all-frames-parameters '((fullscreen . maximized)))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)


(use-package package
  :config
  (package-initialize))

(use-package exwm
  :config
  (setq exwm-workspace-number 5)


  (setq exwm-input-prefix-keys '(?\C-x
                                 ?\C-u
                                 ?\C-h
                                 ?\M-x
                                 ?\M-`
                                 ?\M-:
                                 o?\M-&
                                 ?\C-\M-j
                                 ?\C-\ ))
  (setq exwm-global-keys
        `(([?\s-w] . exwm-workspace-switch)
          ([\?s-h] . windmove-left)
          ([\?s-j] . windmove-down)
          ([\?s-k] . windmove-up)
          ([\?s-l] . windmove-right)
          ((kbd "s-<return>") . (lambda ()
                               (eshell)))

          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  (exwm-enable))

(use-package exwm-randr
  :config
  (exwm-randr-enable))
