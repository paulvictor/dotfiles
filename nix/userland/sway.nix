{config, pkgs, lib, ...}:

{
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    config = {
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
      terminal = "${pkgs.alacritty}/bin/alacritty";
      menu = "${pkgs.fuzzel}/bin/fuzzel";
    };
  };
}
