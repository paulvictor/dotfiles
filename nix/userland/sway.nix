{config, pkgs, lib, ...}:

# Use https://git.sr.ht/~sircmpwn/sway/commit/0d5aaf5359c671a51bd319bd7972e0f5e7bcde84 and implement something like reduce the brightness for inactive windows
let
  modifier = config.wayland.windowManager.sway.config.modifier;
  prefixWithHelper =
    prefix: lib.mapAttrs'
      (n: v: {name = "${prefix}${n}"; value = v;});
  keybindings = prefixWithHelper "${modifier}+" {
    "Shift+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
    "Tab" = "workspace back_and_forth";
    "w" = "kill";
    "e" = "${config.programs.emacs.package}/bin/emacsclient -c -n";
  };
in
{
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    systemd.enable = true;
    config = {
      modifier = "Mod4"; # Super key
      keybindings = lib.mkOptionDefault keybindings;
      output = {
        "HDMI-A-1" = {
          mode = "2560x1440@60Hz";
          transform = "270";
        };
        "DP-1" = {
          mode = "2560x1440@60Hz";
        };
      };
      focus = {
        newWindow = "smart";
        followMouse = "no";
        wrapping = "yes";
        mouseWarping = "container";
      };
      gaps = {
        smartGaps = true;
        smartBorders = "on";
      };
      workspaceAutoBackAndForth = true;
      terminal = "${config.programs.emacs.package}/bin/emacsclient -c -n -e \"(pvr/new-eshell-window)\"";
      menu = "${pkgs.fuzzel}/bin/fuzzel";
      bars = [];
      startup = [
        { command = "systemctl --user restart waybar"; always = true; }
      ];
    };
  };
}
