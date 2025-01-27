{config, pkgs, lib, ...}:
with pkgs;
{
  services.picom = {
    enable = false;
    menuOpacity = 0.7;
    backend = "glx";
    settings = {
      extraOptions = ''
        focus-exclude = (x = 0 && y = 0 && override-redirect = true) || (_NET_WM_NAME@:s = "rofi");
      '';
      noDockShadow = true;
      inactive-dim-fixed = true;
      inactive-dim = 0.4;
      blur = true;
      wintypes =  { dock = { shadow = false; clip-shadow-above = true; }; };
    };
  };

  services.sxhkd = {
    enable = true;
    extraOptions =  [ "-m" " -1" ];
    keybindings = {
      "hyper + shift + Return" = "${rxvt-unicode}/bin/urxvt";
      "hyper + e" = "emacsclient -c -n -d :0";
      "hyper + d" = "rofi -show drun";
      "hyper + p" = "${passdo}/bin/passdo";
      "hyper + s" = "${scrot}/bin/scrot -m";
      "hyper + shift + s" = "${scrot}/bin/scrot -s";
      "hyper + shift + slash" = "menu-surfraw";
      "hyper + shift + d" = "${rofiElectronAppsRunner}/bin/rofiElectronAppsRunner";
      # OCR a screen selection
      "hyper + x" = "${imagemagick}/bin/convert x: -modulate 100,0 -resize 400% -set density 300 png:- | ${tesseract}/bin/tesseract stdin stdout | ${xclip}/bin/xclip -selection clipboard";
      # Pulse Audio controls
      "XF86Audio{Raise,Lower}Volume" = "${pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ {+5%,-5%}"; #increase sound volume
      "XF86AudioMute" =  "${pulseaudio}/bin/pactl set-sink-mute  @DEFAULT_SINK@ toggle"; # mute sound
      # "XF86MicMute" =  "pulseaudio-ctl mute-input"; # mute mic
      # Sreen brightness controls
      "XF86MonBrightness{Up,Down}" = "${light}/bin/light -{A,U} 5";
    };
  };
  xsession = {
    enable = false;
    initExtra = ''
      ${xorg.xmodmap}/bin/xmodmap -verbose ${config.home.homeDirectory}/.Xmodmap
      ${feh}/bin/feh --bg-scale ${wall1} ${wall2} ${wall3}
      ${autorandr}/bin/autorandr --change
    '';
    windowManager.command = "${import ./scripts/mk-stumpwm.nix { inherit pkgs;}}";
  };
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = "${xsecurelock}/bin/xsecurelock";
    xautolock.enable = false;
  };
  programs.autorandr = {
    hooks.postswitch = {
      change-background = "${feh}/bin/feh --bg-scale ${wall1} ${wall2} ${wall3}";
    };
  };
}
