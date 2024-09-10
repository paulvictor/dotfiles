{config, pkgs, lib, ...}:

# Use https://git.sr.ht/~sircmpwn/sway/commit/0d5aaf5359c671a51bd319bd7972e0f5e7bcde84 and implement something like reduce the brightness for inactive windows
let
  modifier =
    config.wayland.windowManager.sway.config.modifier;
  rofiElectronAppsRunner =
    pkgs.callPackage ../overlays/electronApps/rofiRun.nix {};

  prefixWithHelper =
    prefix: lib.mapAttrs'
      (n: v: {name = "${prefix}${n}"; value = v;});
  warpdCommand =
    "${pkgs.warpd}/bin/warpd -c ${config.xdg.configFile."warpd/config".target} ";
  keybindings = prefixWithHelper "${modifier}+" {
    "Shift+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
    "Tab" = "workspace back_and_forth";
    "w" = "kill";
    "BackSpace" = "exec swaylock-fancy";
    "Shift+space" = "focus mode_toggle";
    "u" = "focus parent";
    "Shift+u" = "focus child";
    "space" = "layout toggle stacking tabbed split";
    "t" = "layout toggle split";
    "e" ="exec ${config.programs.emacs.package}/bin/emacsclient -c -n";
    "p" = "exec ${pkgs.passdo}/bin/passdo";
    "s" = "exec ${pkgs.scrot}/bin/scrot -m";
    "Shift+s" = "exec ${pkgs.scrot}/bin/scrot -s";
    "Shift+d" = "exec ${rofiElectronAppsRunner}/bin/rofiElectronAppsRunner";
    "Ctrl+semicolon" = "exec ${pkgs.warpd}/bin/warpd --oneshot --hint2";
    "Ctrl+colon" = "exec ${warpdCommand} --oneshot --grid";
    "Ctrl+Backspace" = "exec ${warpdCommand} --normal";
    "Ctrl+Return" = "exec ${warpdCommand} --screen";

    "Alt+h" = "move workspace to output left";
    "Alt+j" = "move workspace to output down";
    "Alt+k" = "move workspace to output up";
    "Alt+l" = "move workspace to output right";
#     "Shift+slash" = "menu-surfraw";
    # OCR a screen selection
#     "hyper + x" = "${pkgs.imagemagick}/bin/convert x: -modulate 100,0 -resize 400% -set density 300 png:- | ${pkgs.tesseract}/bin/tesseract stdin stdout | ${pkgs.xclip}/bin/xclip -selection clipboard";
    # Pulse Audio controls
#     "XF86Audio{Raise,Lower}Volume" = "${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ {+5%,-5%}"; #increase sound volume
#     "XF86AudioMute" =  "${pkgs.pulseaudio}/bin/pactl set-sink-mute  @DEFAULT_SINK@ toggle"; # mute sound
    # "XF86MicMute" =  "pulseaudio-ctl mute-input"; # mute mic
    # Sreen brightness controls
#     "XF86MonBrightness{Up,Down}" = "${light}/bin/light -{A,U} 5";
  };

in
{
  imports = [ ./services/sxhkd.nix ];
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
        { command = "systemctl --user restart emacs.service"; always = false; }

      ];
    };
  };
}
