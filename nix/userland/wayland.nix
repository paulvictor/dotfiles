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
  ];

  programs.swaylock = {
    enable = true;
    settings = {
      image = pkgs.wall1;
      color = "1a1a1a";
      font-size = 24;
      show-failed-attempts = true;
      scaling = "fill";
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
    events = { "before-sleep" = lockCommand; };
  };

  programs.swayr = {
    enable = true;
    # TODO use fuzzel
  };
  services.mako.enable = true;
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    systemd.targets = ["sway-session.target"];
    style = builtins.readFile ./config/waybar-style.css;
    settings = {
      mainBar = {
        output = [ "*" ];
        layer = "top";
        position = "top";
        height = 30;
        modules-left = [ "sway/workspaces" "sway/mode" ];
        modules-center = [ "sway/window" ];
        modules-right = [ "cpu" "memory" "network" "clock" "battery" ];
        clock = {
          interval = 5;
          tooltip = false;
          format = "{:%a, %d/%m/%Y %R}";
        };
        "sway/mode" = {
          format = " {}";
          max-length = 20;
        };
        cpu = {
          interval = 10;
          max-length = 10;
          format = "   {usage}%";
        };
        memory = {
		      format = " 💾 {used:0.1f}G";
	      };
        battery =  {
          bat =  lib.mkDefault "BAT0";
          states = {
            good =  95;
            warning =  30;
            critical =  15;
            };
          format = "{icon} {capacity}%";
          format-icons =  ["" "" "" "" ""];
        };
        network = {
		      format-wifi = "<span color='#589df6'></span> <span color='gray'>{essid}</span> <span color='#589df6'> {signalStrength} % </span> <span color='#589df6'>⇵</span> {bandwidthUpBits}/{bandwidthDownBits}";
#           "format-wifi" =  "{essid} ({signalStrength}%) ";
#           "format-ethernet" =  "{ifname} =  {ipaddr}/{cidr} ";
          format-ethernet = "{ifname}: {ipaddr}/{cidr} ";
		      format-linked = "{ifname} (No IP) ";
          format-disconnected =  "Disconnected ⚠";
        };
      };
    };
  };
  services.stumpwm-like.enable = false;
}
