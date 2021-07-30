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
      #"number \"${lib.elemAt workspaceNumbers 1}\"" = [{ class = "Vivaldi-stable"; }];
      "number \"${lib.elemAt workspaceNumbers 5}\"" = [{ class = "Surf"; }];
    };
    modifier = "Mod4";
    keybindings = useWithModifier modifier ({
      # kill focused window
      "a" = "focus child";
      #"asciitilde" = "move container to workspace back_and_forth";
      "BackSpace" = "mode $leave";
      "bar" = "split horizontal";
      "braceleft" = "move workspace to output down";
      "braceright" = "move workspace to output up";
      "b" = "move workspace to output left";
      "Shift+b" = "move workspace to output right";
      "less" = "move workspace to output left";
      "greater" = "move workspace to output right";
      "c" = "reload";
      "e" = "layout toggle stacking tabbed splith splitv";
      "equal" = "scratchpad show";
      "f" = "fullscreen toggle";
      "grave" = "workspace back_and_forth";
      "h" = "focus left";
      "j" = "focus down";
      "k" = "focus up";
      "l" = "focus right";
      "minus" = "split vertical";
      "q" = "kill";
      "r" = "mode resize";
      "Shift+a" = "focus parent";
      "Shift+c" = "restart";
      "Shift+equal" = "move scratchpad";
      "Shift+h" = "move left";
      "Shift+j" = "move down";
      "Shift+k" = "move up";
      "Shift+l" = "move right";
      "slash" = ''exec quickswitch.py -d "rofi -i -dmenu"'';
      "s" = "layout stacking";
      "Space" = "focus mode_toggle";
      "t" = "split toggle";
      "v" = "exec ${shareLink}/bin/shareLink";
      "w" = "layout tabbed";
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
#      {
#        command = ''
#          i3-msg 'rename workspace 1 to "1: code"'
#        '';
#        always = true;
#        notification = false;
#      }
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
      modifier = "Mod1";
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
