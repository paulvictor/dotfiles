# Basic fonts for the system. Fonts a program needs are installed by that program's feature
# (home-manager `home.packages`; the `fonts` home-manager half only turns fontconfig on for them).
{
  flake.modules.nixos.fonts = { pkgs, ... }: {
    fonts.fontconfig.enable = true;
    fonts.packages = with pkgs; [
      dejavu_fonts.full-ttf
      noto-fonts
      ubuntu-classic
      unifont
      nerd-fonts.dejavu-sans-mono
    ];
  };

  flake.modules.homeManager.fonts.fonts.fontconfig.enable = true;
}
