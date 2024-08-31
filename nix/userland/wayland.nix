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
    enable = true;
    timeouts = [
      { timeout = 300; command = "${lockPackage}/bin/swaylock-fancy"; }
      {
        timeout = 500;
        command = "${sway}/bin/swaymsg \"output * dpms off\"";
        resumeCommand = "${sway}/bin/swaymsg \"output * dpms on\"";
      }
      { timeout = 600; command = "systemctl suspend"; }
    ];
    events = [
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
    systemd.enable = true;
    systemd.target = "sway-session.target";
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 15;
        modules-left = [ "sway/workspaces" "sway/mode" "wlr/taskbar" ];
        modules-center = [ "sway/window" ];
        modules-right = [ "cpu" "network" "clock" "battery" ];
        clock = {
          interval = 5;
          tooltip = false;
          format = "{:%d-%m-%Y | %a | %R %p}";
        };
        "sway/mode" = {
          format = "<span style=\"italic\">{}</span>";
        };
        cpu = {
          interval = 10;
          max-length = 10;
          "format" = "{usage}% ";

        };
        "battery" =  {
            "bat" =  "BAT0";
            "states" = {
                "good" =  95;
                "warning" =  30;
                "critical" =  15;
            };
            "format" = "{capacity}% {icon}";
            "format-icons" =  ["" "" "" "" ""];
        };
        "network" = {
#             // "interface" =  "wlp2s0"; // (Optional) To force the use of this interface
            "format-wifi" =  "{essid} ({signalStrength}%) ";
            "format-ethernet" =  "{ifname} =  {ipaddr}/{cidr} ";
            "format-disconnected" =  "Disconnected ⚠";
        };
      };
    };
  };
}
