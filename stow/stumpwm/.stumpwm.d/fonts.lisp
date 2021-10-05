; git clone https://github.com/goose121/clx-truetype into ~/quicklisp/local-projects before this
(load-module "ttf-fonts")
(in-package :stumpwm)

(setf xft:*font-dirs*
      '("/home/viktor/.nix-profile/share/fonts/opentype/"
        "/home/viktor/.nix-profile/share/fonts/truetype/"))

(unless (xft:get-font-families)
  (xft:cache-fonts))

(set-font (list
           (make-instance 'xft:font
                          :family "VictorMono Nerd Font"
                          :subfamily "Bold"
                          :size 12)))

