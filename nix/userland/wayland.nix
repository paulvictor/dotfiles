{ lib, config, pkgs, ...}:

{
  programs.swaylock.enable = true;
  programs.swayr.enable = true;
  programs.wofi.enable = true;

  home.packages = with pkgs; [
    mako
    wl-clipboard
    shotman
  ];

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    config = rec {
      terminal = "${pkgs.alacritty}/bin/alacritty";
      menu = "wofi --show run";
      modifier = "Mod4"; # Super key
      output = {
        "HDMI-A-1" = {
          mode = "2560x1440@60Hz";
          transform = "270";
        };
        "DP-1" = {
          mode = "2560x1440@60Hz";
        };
      };
    };
  };
}
