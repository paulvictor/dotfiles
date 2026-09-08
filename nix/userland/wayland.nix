{ lib, config, pkgs, specialArgs, ...}:

let
  lockCommand = "${pkgs.swaylock}/bin/swaylock -F -f -c 000000";
in
{
  programs.wofi.enable = true;

  home.packages = with pkgs; [
    wl-clipboard
    shotman
    wdisplays
    wl-mirror
  ];

  imports = [
    ./services/guile-swayer.nix
    ./sway.nix
    ./mako.nix
    ./waybar.nix
  ];

  programs.swaylock = {
    enable = true;
    settings = {
      image = pkgs.wall1;
      color = "1a1a1a";
      font-size = 24;
      show-failed-attempts = true;
      scaling = "fill";
      # Optional: High-DPI optimizations for the Surface screen
      indicator-radius = 100;
      indicator-thickness = 7;
      ring-color = "3d59a1";
      key-hl-color = "82a1f1";
    };
  };

  services.wpaperd = {
    enable = true;
    settings = {
      default = {
        path = pkgs.wall1;
        mode = "stretch";
      };
    };
  };

  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        terminal = "${pkgs.alacritty}/bin/alacritty";
        width = 30;
      };
      border = { width = 3; };
      colors = {
        background = "#2d2d329b";
        text = "#f0f0f0ff";
        match = "#63b5f6ff";
        selection-match = "#8be9fdff";
        selection = "#44475add";
        selection-text = "#f8f8f2ff";
        border = "#F9E2AFff";
      };
    };
  };

  services.swayidle = with pkgs;{
    enable = lib.mkDefault true;
    timeouts = [
      { timeout = 300; command = lockCommand; }
    ] ++ (
      lib.optionals (specialArgs.hostname != "anarki") [ # On this machine dont switch off the monitor
        {
          timeout = 500;
          command = "${sway}/bin/swaymsg \"output * dpms off\"";
          resumeCommand = "${sway}/bin/swaymsg \"output * dpms on\"";
        }
      ]
    );
    events = {
      "before-sleep" = "${lockCommand} && ${sway}/bin/swaymsg \"output * dpms off\"";
      "after-resume" = "${sway}/bin/swaymsg \"output * dpms on\"";
    };
  };

  programs.swayr = {
    enable = true;

  };
  services.mako.enable = true;
  services.stumpwm-like.enable = false;
}
