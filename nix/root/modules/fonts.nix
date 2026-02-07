{config, pkgs, lib, ...}:

{
  fonts.fontconfig.enable = true;
  fonts.packages = with pkgs;[
    emacs-all-the-icons-fonts
    cantarell-fonts
    dejavu_fonts.full-ttf
    font-awesome
    noto-fonts
    ubuntu-classic
    unifont
  ] ++ (with pkgs.nerd-fonts;
    [
      #hack
      victor-mono
      jetbrains-mono
      udev-gothic-nf
      plemoljp-nf
      iosevka-term
      #iosevka
      sauce-code-pro
      dejavu-sans-mono
      symbols-only
      #fira-code
      monoid
    ]);
}
