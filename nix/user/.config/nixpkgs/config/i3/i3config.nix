{ pkgs, rofiElectronAppsRunner, shareLink }:

with pkgs;
let
  #workspaces = ["λ" "" "🗩" "" "♫" "⚙" "" "🗎" ""];
  workspaces = ["code" "web" "messenger" "admin" "docs" "search" "scratch" "scratch" "scratch"];
  numbers = map toString (lib.range 1 9);
  workspaceNumbers = lib.zipListsWith (x: y: x + ": " + y) numbers workspaces ;
  useWithModifier = mod: lib.mapAttrs' (k: v: lib.nameValuePair (mod + "+" + k) v);
  changeToi3ExitOption = lib.mapAttrs (k: v: "exec --no-startup-id i3exit ${v}, mode \"default\"");
in
{
  config = rec {
    fonts = [
      "Noto Sans Symbols 10"
      "Noto Sans Symbols2 10"
      "Noto Sans Mono 10"
      "SauceCodePro Nerd Font 10"
    ];
    assigns = {
      "number \"${lib.elemAt workspaceNumbers 4}\"" = [{ class = "qutebrowser"; }];
      "number \"${lib.elemAt workspaceNumbers 1}\"" = [{ class = "Vivaldi-stable"; }];
      "number \"${lib.elemAt workspaceNumbers 5}\"" = [{ class = "Surf"; }];
    };
    modifier = "Mod4";
    keybindings = useWithModifier modifier ({
      "Return" = "exec ${termite}/bin/termite";
      "Shift+Return" = "exec ${termite}/bin/termite -e tmux";
# kill focused window
      "q" = "kill";
      "h" = "focus left";
      "j" = "focus down";
      "k" = "focus up";
      "l" = "focus right";
      #"g" = ''exec ${wmfocus}/bin/wmfocus --fill --bgcolor "rgba(30, 30, 30, 0.5)"  -c asdf --textcolor red'';
      "t" = "split toggle";
      "f" = "fullscreen toggle";
      "s" = "layout stacking";
      "w" = "layout tabbed";
      "e" = "layout toggle stacking tabbed splith splitv";
# change focus between tiling / floating windows
      "space" = "focus mode_toggle";
      "Shift+space" = "floating toggle";
      "a" = "focus child";
      "Shift+a" = "focus parent";
# reload the configuration file
      "c" = "reload";
# restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
      "Shift+c" = "restart";
      "r" = "mode resize";
      "BackSpace" = "mode $leave";
      "minus" = "split vertical";
      "bar" = "split horizontal";
      "Shift+h" = "move left";
      "Shift+j" = "move down";
      "Shift+k" = "move up";
      "Shift+l" = "move right";
      "grave" = "workspace back_and_forth";
      #"asciitilde" = "move container to workspace back_and_forth";
      "d" = "exec rofi -show drun";
      "Shift+d" = "exec ${rofiElectronAppsRunner}/bin/rofiElectronAppsRunner";
      "slash" = ''exec quickswitch.py -d "rofi -i -dmenu"'';
      "Shift+slash" = ''exec menu-surfraw'';
      "v" = "exec ${shareLink}/bin/shareLink";
    } //
    lib.foldl (x: y: x // y) {}
      (lib.zipListsWith
        (i: n: {
          "${i}" = "workspace number ${n}";
          "Shift+${i}" = "move container to workspace number ${n}";
        })
        numbers
        workspaceNumbers)
    );
    keycodebindings = useWithModifier modifier {
    };
    modes = {
      "$leave" = changeToi3ExitOption {
        l = "lock";
        e = "logout";
        s = "suspend";
        h = "hibernate";
        r = "reboot"; } // {
        Return = "mode default";
        Escape = "mode default";
      };
      resize = {
        h = "resize shrink width 3 px or 3 ppt";
        j = "resize shrink height 3 px or 3 ppt";
        k = "resize grow height 3 px or 3 ppt";
        l = "resize grow width 3 px or 3 ppt";
        Return = "mode default";
        Escape = "mode default";
      };
    };
    bars = [];
    startup = [
      {
        command = "systemctl --user restart polybar";
        always = true;
        notification = false;
      }
      {
        command = "nm-applet";
        always = true;
        notification = false;
      }
      {
        command = ''
          i3-msg 'rename workspace 1 to "1: code"'
        '';
        always = true;
        notification = false;
      }
    ];
    colors = {
      focused = {
        border = "#586e75";
        background = "#586e75";
        indicator = "#fdf6e3";
        childBorder = "#268bd2";
        text = "";
      };
      focusedInactive = {
        border = "#073642";
        background = "#073642";
        indicator = "#93a1a1";
        childBorder = "#002b36";
        text = "";
      };
      unfocused = {
        border = "#002b36";
        background = "#002b36";
        indicator = "#586e75";
        childBorder = "#002b36";
        text = "";
      };
      urgent = {
        border = "#dc322f";
        background = "#dc322f";
        indicator = "#fdf6e3";
        childBorder = "#dc322f";
        text = "";
      };
    };
    focus = {
      followMouse = false;
      forceWrapping = true;
      mouseWarping = false;
    };
    window = {
      border = 0;
      hideEdgeBorders = "both";
      commands = [
        { criteria = { class = "Electron"; title = ".*Slack.*"; }; command = "move container to workspace ${lib.elemAt workspaceNumbers 2}";}
        { criteria = { class = "Electron"; title = ".*WhatsApp.*"; }; command = "move container to workspace ${lib.elemAt workspaceNumbers 2}";}
        { criteria = { class = "Electron"; title = ".*Gitter.*"; }; command = "move container to workspace ${lib.elemAt workspaceNumbers 2}";}
        { criteria = { class = "Electron"; title = ".*Hangouts.*"; }; command = "move container to workspace ${lib.elemAt workspaceNumbers 2}";}
      ];
    };
    floating = {
      border = 0;
      criteria = [
        { class = "Electron"; }
        { class = "Pavucontrol"; }
        { class = "Skype"; }
        { class = "pinentry"; }
        { window_role = "pop-up"; }
        { window_role = "task_dialog"; }
        { title = "Preferences"; }
      ];
    };
  };
  enable = true;
  package = i3-gaps;
  extraConfig = builtins.readFile ./i3config;
}
