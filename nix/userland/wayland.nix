{ lib, config, pkgs, ...}:

let
  lockPackage =
    with pkgs;
    swaylock-fancy.overrideAttrs(prev: {
    postInstall = ''
      wrapProgram $out/bin/swaylock-fancy \
        --prefix PATH : ${lib.makeBinPath [ coreutils grim gawk jq swaylock imagemagick getopt fontconfig wmctrl sway ]}
    '';
  });

in
{

  programs.wofi.enable = true;

  home.packages = with pkgs; [
    wl-clipboard
    shotman
  ];

  imports = [ ./sway.nix ];

  programs.swaylock = {
    enable = true;
    package = lockPackage;
  };

  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        terminal = "${pkgs.alacritty}/bin/alacritty";
        "dpi-aware" = "auto";
        width = 25;
        "inner-pad" = 0;
      };
      border= {width = 3;};
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
    enable = true;
    timeouts = [
      { timeout = 300; command = "${lockPackage}/bin/swaylock-fancy"; }
      { timeout = 500; command = "${sway}/bin/swaymsg \"output * dpms off\""; }
    ];
    events = [
      { event = "after-resume"; command = "${sway}/bin/swaymsg \"output * dpms on\""; }
      { event = "before-sleep"; command = "${lockPackage}/bin/swaylock-fancy";}
      { event = "lock"; command = "${lockPackage}/bin/swaylock-fancy";}
    ];
  };

  programs.swayr = {
    enable = true;
    # TODO use fuzzel
  };
  services.mako.enable = true;
  programs.waybar = {
    enable = true;
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 20;
        output = [ "eDP-1" ];
        modules-left = [ "sway/workspaces" "sway/mode" "wlr/taskbar" ];
        modules-center = [ "sway/window" ];
        modules-right = [ "network" "clock" "battery"];
        clock = {
          interval = 5;
          tooltip = true;
          format = "{:%H.%M}";
          tooltip-format = "{:%d-%m-%Y}";
        };
      };
    };
  };
}
