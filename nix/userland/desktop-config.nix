{ config, pkgs, lib, specialArgs, ... }:

with lib;
mkIf
  specialArgs.isDesktop
  (let
    rofiElectronAppsRunner = pkgs.callPackage ../overlays/electronApps/rofiRun.nix {};
    onAttachMonitorScript = pkgs.callPackage ./scripts/onAttachMonitor.nix {};
    wmexit = pkgs.callPackage ./scripts/wmexit.nix {};
    passdo = pkgs.callPackage ./scripts/passdo.nix {};
    quickswitch-for-i3 = pkgs.callPackage ./packages/quickswitch-for-i3 {};
    findWindowByTitle = pkgs.callPackage ./scripts/findWindowByTitle.nix {};
  in
    with pkgs;
    {
      services.gocryptfs = {
        enable = true;
        cryptDir = "${config.home.homeDirectory}/crypt";
        plainDir = "${config.home.homeDirectory}/plain";
        passCmd = "cat /run/secrets/crypt-mount-key";
      };

      services.flameshot.enable = true;

#   services.network-manager-applet.enable = true;
#   services.pasystray.enable = true;
      home.packages = [
        acpi # TODO : Install only on laptops
        gnome.adwaita-icon-theme
        afuse
        arandr
        asciinema
        dunst
        findWindowByTitle
        wmexit
        pa_applet
        paprefs
        passdo
#         pasystray
        pavucontrol
        pulseaudio-ctl
        rofiElectronAppsRunner
        wmctrl
        wmfocus
        xclip
        xdotool
        xorg.xdpyinfo
        xorg.xmodmap
        xsel
      ];

      home.persistence."/persist/home/viktor" = {
        allowOther = false;
        directories = [
          "Downloads"
          "crypt"
        ];
      };

      xdg.configFile.rofi = {
        recursive = true;
        source = fetchFromGitHub {
          owner = "davatorium";
          repo = "rofi-themes";
          rev = "bfdde8e7912ad50a468c721b29b448c1ec5fa5e3";
          sha256 = "w/AE1o8vIZdD0jAi7++gdlmApGjeyDv6CD4xxrD9Fsw=";
        };
      };

      services.dunst = {
        enable = true;
        settings = import ./config/dunstrc.nix;
        iconTheme = {
          name = "Numix";
          package = numix-icon-theme;
          size = "48";
        };
      };
      xsession = {
        enable = true;
        initExtra = ''
          sleep 3 && ${xorg.xmodmap}/bin/xmodmap -verbose ${config.home.homeDirectory}/.Xmodmap
          ${feh}/bin/feh --bg-scale ${wall1} ${wall2} ${wall3}
        '';
        windowManager.command = "${import ./scripts/mk-stumpwm.nix { inherit pkgs;}}";
      };

      fonts.fontconfig.enable = true;

      services.picom = {
        enable = true;
        backend = "xrender";
        blur = true;
        fadeDelta = 0;
        menuOpacity = "0.7";
        noDockShadow = true;
        inactiveDim = "0.2";
        experimentalBackends = true;
        extraOptions = ''
      inactive-dim-fixed = true;
      #focus-exclude = (x = 0 && y = 0 && override-redirect = true) || (_NET_WM_NAME@:s = "rofi");
    '';
      };

      services.sxhkd = {
        enable = true;
        extraOptions =  [ "-m" " -1" ];
        keybindings = {
          #       "control + hyper + alt + Return" = "${alacritty}/bin/alacritty";
          "control + hyper + alt + shift + Return" = "${alacritty}/bin/alacritty";
          "control + hyper + alt + e" = "emacsclient -c -n";
          "control + hyper + alt + d" = "rofi -show drun";
          #       "control + hyper + alt + g" = ''${wmfocus}/bin/wmfocus --fill -c asdf --textcolor red'';
          "control + hyper + alt + n" = "passdo --notify";
          "control + hyper + alt + p" = "passdo --copy";
          "control + hyper + alt + s" = "scrot -m";
          "control + hyper + alt + shift + s" = "scrot -s";
          "control + hyper + alt + shift + p" = "passdo --type";
          "control + hyper + alt + shift + slash" = "menu-surfraw";
          "control + hyper + alt + shift + d" = "${rofiElectronAppsRunner}/bin/rofiElectronAppsRunner";
          # OCR a screen selection
          "control + hyper + alt + x" = "${imagemagick}/bin/convert x: -modulate 100,0 -resize 400% -set density 300 png:- | ${tesseract}/bin/tesseract stdin stdout | ${xclip}/bin/xclip -selection clipboard";
          # Pulse Audio controls
          "XF86Audio{Raise,Lower}Volume" = "${pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ {+5%,-5%}"; #increase sound volume
          "XF86AudioMute" =  "${pulseaudio}/bin/pactl set-sink-mute  @DEFAULT_SINK@ toggle"; # mute sound
          # "XF86MicMute" =  "pulseaudio-ctl mute-input"; # mute mic
          # Sreen brightness controls
          "XF86MonBrightness{Up,Down}" = "${light}/bin/light -{A,U} 5";
        };
      };
      programs.alacritty = {
        enable = true;
        settings = {
          window.startup_mode = "Maximized";
          window.decorations = "none";
          window.title = "Alacritty";
          window.gtk_theme_variant = "dark";
          scrolling.multiplier = 10;
          font.normal.family = "VictorMono Nerd Font Mono";
          font.normal.style = "SemiBold";
          font.bold.family = "VictorMono Nerd Font Mono";
          font.bold.style = "Bold";
          font.italic.family = "VictorMono Nerd Font Mono";
          font.italic.style = "SemiBold Italic";
          font.bold_italic.family = "VictorMono Nerd Font Mono";
          font.bold_italic.style = "Bold Italic";
          font.size = 12.0;
          bell.animation = "EaseOutSine";
          selection.save_to_clipboard = true;
          cursor.style = "Block";
          cursor.blinking = "Always";
          cursor.blink_interval = 500;
          cursor.vi_mode_style = "Block";
          key_bindings = [
            { key = "V"; mods = "Control|Shift"; action = "Paste"; }
            { key = "C"; mods = "Control|Shift"; action = "Copy"; }
            { key = "Space"; mods = "Control"; action = "ToggleViMode"; }
            { key = "Space"; mods = "Shift"; action = "ToggleViMode"; }
            { key = "I"; mode = "Vi"; action = "ToggleViMode"; }
            { key = "Escape"; mode = "Vi"; action = "ClearSelection"; }
            { key = "H"; mode = "Vi"; action = "Left"; }
            { key = "J"; mode = "Vi"; action = "Down"; }
            { key = "K"; mode = "Vi"; action = "Up"; }
            { key = "L"; mode = "Vi"; action = "Right"; }
            { key = "Y"; mode = "Vi"; action = "Copy"; }
            { key = "Y"; mode = "Vi"; action = "ClearSelection"; }
            { key = "B"; mods = "Shift"; mode = "Vi"; action = "ScrollToBottom"; }
            { key = "B"; mode = "Vi"; action = "ScrollToTop"; }
            { key = "B"; mods = "Control"; mode = "Vi"; action = "ScrollPageUp"; }
            { key = "F"; mods = "Control"; mode = "Vi"; action = "ScrollPageDown"; }
            { key = "U"; mods = "Control"; mode = "Vi"; action = "ScrollHalfPageUp"; }
            { key = "D"; mods = "Control"; mode = "Vi"; action = "ScrollHalfPageDown"; }
            { key = "Slash"; mods = "Shift"; mode = "Vi"; action = "SearchBackward"; }
            { key = "Slash"; mode = "Vi"; action = "SearchForward"; }
            { key = "N"; mods = "Shift"; mode = "Vi"; action = "SearchPrevious"; }
            { key = "N"; mode = "Vi"; action = "SearchNext"; }
          ];
          draw_bold_text_with_bright_colors = true;
          window.opacity = 0.8;
          colors1 = {
            primary = {
              background = "0x181818";
              foreground = "0xd8d8d8";
            };
            cursor = {
              text = "0xd8d8d8";
              cursor = "0xd8d8d8";
            };
            normal = {
              black =   "0x181818";
              red =     "0xab4642";
              green =   "0xa1b56c";
              yellow =  "0xf7ca88";
              blue =    "0x7cafc2";
              magenta = "0xba8baf";
              cyan =    "0x86c1b9";
              white =   "0xd8d8d8";
            };
            bright = {
              black =   "0x585858";
              red =     "0xab4642";
              green =   "0xa1b56c";
              yellow =  "0xf7ca88";
              blue =    "0x7cafc2";
              magenta = "0xba8baf";
              cyan =    "0x86c1b9";
              white =   "0xf8f8f8";
            };
          };
        };
      };
      xdg.configFile."kitty/kitty.conf".source = import ./config/kitty.nix { inherit pkgs; };
#       xdg.configFile."pulse/default.pa".text = lib.readFile ./config/pulseaudio.conf;
      xdg.configFile."rofi/config.rasi".source = import ./config/rofi-config.nix {inherit pkgs; };

      # Not sure if we can run on darwin
      services.gpg-agent = {
        enable = true;
        enableExtraSocket = true;
        enableSshSupport = true;
        defaultCacheTtl = 3600;
        defaultCacheTtlSsh = 3600;
        maxCacheTtl = 6 * 3600;
        maxCacheTtlSsh = 6 * 3600;
        sshKeys = [
          "# id_rsa"
          "1F525695DC054E59E3D357E2F76F05DE63F2AD0D"
        ];
      };

      systemd.user.services.xss-lock = {
        Unit = {
          Description = "xss-lock, session locker service";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Install = { WantedBy = [ "graphical-session.target" ]; };

        Service = {
          Environment="PATH=/run/wrappers/bin:${coreutils}/bin";
          ExecStart =
            lib.concatStringsSep " "
              ([ "${pkgs.xss-lock}/bin/xss-lock" "-s \${XDG_SESSION_ID}" ]
               ++ [ "--notifier" "${libnotify}/bin/notify-send" ]
               ++ [ "-- ${wmexit}/bin/wmexit lock" ]);
        };
      };

      xdg.configFile."zathura/zathurarc".text = ''
        set selection-clipboard clipboard
      '';
      home.file.".Xmodmap".text =
        lib.concatStringsSep "\n"
          [ # (builtins.readFile ./config/apl.xmodmap)
            (builtins.readFile ./config/xmodmap)];
      systemd.user.services.yubikey-touch-detector = {
        Install.WantedBy = [ "default.target" ];
        Unit.Description = "Detects when your YubiKey is waiting for a touch";
        Service =
          let
            ytd = pkgs.callPackage ./packages/ytd {};
          in {
            Environment="PATH=${pkgs.gnupg}/bin:/run/wrappers/bin";
            ExecStart = "${ytd}/bin/yubikey-touch-detector --libnotify";
            Type = "simple";
          };
      };
      xdg.mimeApps = {
        enable = true;
        defaultApplications = {
          "x-scheme-handler/http" = [ "firefox.desktop" ];
          "x-scheme-handler/https" = [ "firefox.desktop" ];
          "x-scheme-handler/chrome" = [ "firefox.desktop" ];
          "text/html" = [ "firefox.desktop" ];
          "application/x-extension-htm" = [ "firefox.desktop" ];
          "application/x-extension-html" = [ "firefox.desktop" ];
          "application/x-extension-shtml" = [ "firefox.desktop" ];
          "application/xhtml+xml" = [ "firefox.desktop" ];
          "application/x-extension-xhtml" = [ "firefox.desktop" ];
          "application/x-extension-xht" = [ "firefox.desktop" ];
        };
      };
      home.file.".urlview".text = ''
    SHORTCUT
    NOREVIEW
    RAW_RESERVED
  '';
    }
  )

