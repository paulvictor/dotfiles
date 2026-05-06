{ config, pkgs, lib, specialArgs, ... }:

with pkgs;
let
  custom-vieb = import ./packages/vieb.nix { inherit pkgs; inherit (config.home) homeDirectory; };

  menu-surfraw = pkgs.callPackage ./scripts/menu-surfraw.nix {};
in
{
  imports = [ ./firefox.nix ];
  config = {
    home.packages =
      [
        autorandr
        brotab
        electronApps
        hicolor-icon-theme
        league-of-moveable-type
        localsend
        material-icons
        mpv
        pdftk
        siji
        zathura # Crashing.
      ]
      ++ (lib.optionals pkgs.stdenv.isx86_64
        [
          google-chrome
        ]
      )
      ++ (lib.optionals pkgs.stdenv.isLinux
        [
          menu-surfraw
          rofi
          scrot
          surfraw
          ungoogled-chromium
          vlc
          yubico-piv-tool
          yubikey-manager
          yubikey-personalization
        ]
      );
    home.file.".vieb/colors/gruvbox.css".source = ./config/vieb/colors/gruvbox.css;
    home.file.".vieb/colors/smalltabs.css".source = ./config/vieb/colors/smalltabs.css;
    home.file.".vieb/colors/dark_minimal.css".source = ./config/vieb/colors/dark_minimal.css;

  };
}
