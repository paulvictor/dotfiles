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
  # obsolete, done with guile swayer
  keybindings = prefixWithHelper "${modifier}+" {
    "Shift+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
    "Tab" = "workspace back_and_forth";
    "w" = "kill";
    "BackSpace" = "exec swaylock";
    "Shift+space" = "focus mode_toggle";
    "u" = "focus parent";
    "Shift+u" = "focus child";
    "space" = "layout toggle stacking tabbed split";
    "t" = "layout toggle split";
    "e" ="exec ${config.programs.emacs.package}/bin/emacsclient -c -n";
    "p" = "exec ${pkgs.passdo}/bin/passdo";
#     "s" = "exec ${pkgs.scrot}/bin/scrot -m";
#     "Shift+s" = "exec ${pkgs.scrot}/bin/scrot -s";
    "Shift+d" = "exec ${rofiElectronAppsRunner}/bin/rofiElectronAppsRunner";

    "o" =  "exec ${pkgs.warpd} --normal";

    "Alt+h" = "move workspace to output left";
    "Alt+j" = "move workspace to output down";
    "Alt+k" = "move workspace to output up";
    "Alt+l" = "move workspace to output right";
  };

in
{
  imports = [ ./services/sxhkd.nix ];
  home.packages = with pkgs;[ slurp grim wl-clipboard flameshot wlay ];
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    systemd.enable = true;
    extraConfigEarly = ''
      exec_always --no-startup-id wpaperd
    '';
    config = {
      modifier = "Mod4"; # Super key
      keybindings = lib.mkForce {}; #lib.mkOptionDefault keybindings;

      output = {
        "DP-1" = {
          mode = "2560x1440@60Hz";
          pos = "1440 523";
        };
        "DSI-1" = { # TODO, can we do only on bones.
          mode = "1920x1200@60Hz";
          pos = "0 0";
          transform = "90";
          scale = "1.60";
        };
        "HDMI-A-1" = {
          mode = "2560x1440@60Hz";
          transform = "270";
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
        { command = "wpaperd"; always = true; }
        { command = "\"guile ~/stumpwm-like/init.scm\""; always = true; } # TODO make this a systemd service and make it a dependency of sway-session.target
        { command = "systemctl --user restart waybar"; always = true; }
        { command = "systemctl --user restart emacs.service"; always = false; }
        { command = "${pkgs.alacritty}/bin/alacritty"; always = true;}
      ];
    };
  };
}
