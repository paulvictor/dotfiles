# Fonts for the emacs config (stow/emacs/.emacs.d/init.el)
pkgs: with pkgs; {
  # In use: the default face, the icon fallbacks, all-the-icons
  used = [
    nerd-fonts.jetbrains-mono
    nerd-fonts.symbols-only
    emacs-all-the-icons-fonts
  ];
  # Faces from the commented out set-face-attribute lines in init.el, to try out
  trial = [
    udev-gothic-nf
    nerd-fonts.monoid
    nerd-fonts.victor-mono
    nerd-fonts.iosevka-term
    (iosevka-bin.override { variant = "SGr-IosevkaFixedSlab"; })
    julia-mono
  ];
}
