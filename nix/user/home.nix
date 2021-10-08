{pkgs, config, ... }:

let
  comma = import (fetchTarball https://github.com/Shopify/comma/tarball/master) { inherit pkgs; };
  firefox-overlay =
    import
      (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  urxvt-perls = import ./packages/urxvt-perls/overlay.nix;
  electron-apps = import ./packages/electronApps;
  surfraw-overlay = import ./packages/surfraw.nix;
  mpd-overlay = import ./packages/mpd.nix;
  ffmpeg-overlay = import ./packages/ffmpeg.nix;
  wallpaper-overlay = import ./packages/wallpaper.nix;
  compton-git-overlay = import ./packages/compton-git.nix;
  pass-override-overlay = import ./packages/pass-override.nix;
  pass-extensions-overlay = import ./packages/pass-extensions.nix;
  julia-overlay = import ./packages/julia.nix;
  dyalog-nixos = import (fetchTarball https://github.com/markus1189/dyalog-nixos/tarball/3e09260ec111541be3e0c7a6c4e700fc042a3a8a) { inherit pkgs; } ;
  hie = import ./packages/hie.nix { inherit pkgs; } ;
  nixos-generators =
    let srcRepo = pkgs.fetchFromGitHub {
      owner = "nix-community";
      repo = "nixos-generators";
      rev = "450f39121c45b93951c52874fe233455d007d3bc";
      sha256 = "1iwc39hzvzzyndxwbnl3fck7phxnjpnhy8zn4nyp8is1fiw0648v";
      fetchSubmodules = true;
    };
    in import srcRepo;
  agenix = import (fetchTarball "https://github.com/ryantm/agenix/tarball/ddb81b8bdafa83a7aa210fc98ead4c78e7a70912") { inherit pkgs; };
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") { inherit pkgs; };
  betterlockscreen = pkgs.callPackage ./packages/betterlockscreen {};
  xdotool-overlay = import ./packages/xdotool.nix;
  neovim-overlay = import (builtins.fetchTarball {
    url = https://github.com/mjlbach/neovim-nightly-overlay/archive/master.tar.gz;
  });
  ripgrep-overlay = import ./packages/ripgrep.nix;
  rofi-fuzzy = import ./wrappers/rofi-fuzzy.nix;
  brotab-overlay =
    import ./packages/brotab.nix;
  recCallPkgs = dir:
    let content = builtins.readDir dir; in
      builtins.listToAttrs
         (map (n: {name = n; value = pkgs.callPackage (dir + ("/" + n)) {}; })
         (builtins.filter (n: builtins.pathExists (dir + ("/" + n + "/default.nix")))
           (builtins.attrNames content)));
in
with pkgs;
let
  jailed-firefox = import ./packages/jailed-firefox { inherit pkgs; };
  pCloudCC =
    callPackage ./packages/pCloudCC/pCloudCC.nix {};
  obelisk =
    import (fetchTarball https://github.com/obsidiansystems/obelisk/archive/master.tar.gz) {};
  easy-purs =
    import (fetchTarball https://github.com/justinwoo/easy-purescript-nix/tarball/47bdc016c7d56e987ca1aca690b1d6c9816a8584) { inherit pkgs; };
  #pscid =
    #callPackage ./packages/pscid/default.nix {
      #inherit pkgs system;
      #purescript = easy-purs.purescript;
    #};
  rofiElectronAppsRunner =
    import ./packages/electronApps/rofiRun.nix { inherit pkgs rofi electronApps; };
  onAttachMonitor =
    import ./scripts/onAttachMonitor.nix { inherit pkgs config; };
  polybarLaunch =
    import ./scripts/polybarLaunch.nix { inherit pkgs; };
  i3exit =
    import ./scripts/i3exit.nix { inherit pkgs; };
  passdo =
    import ./scripts/passdo.nix { inherit pkgs; };
  quickswitch-for-i3 =
    import ./packages/quickswitch-for-i3 { inherit pkgs; };
  pursuit =
    import ./scripts/pursuit.nix { inherit pkgs; };
  findWindowByTitle =
    import ./scripts/findWindowByTitle.nix { inherit pkgs; };
  batteryWarn =
    callPackage ./scripts/batteryWarn.nix {};
  source-code-pro-nerdfonts =
    callPackage ./packages/source-code-pro-nerdfonts {};
  hack-nerdfonts =
    callPackage ./packages/hack-nerdfonts.nix {};
  firacode-nerdfonts =
    callPackage ./packages/fira-code-nerdfonts.nix {};
  victor-mono-nerdfonts =
    callPackage ./packages/victor-mono-nerdfonts/default.nix {};
  all-the-icons-fonts =
    callPackage ./packages/all-the-icons-fonts.nix {};
  menu-surfraw =
    import ./scripts/menu-surfraw.nix { inherit pkgs; };
  shareLink =
    import ./scripts/shareLink.nix { inherit pkgs config; };
  pms =
    callPackage ./packages/pms {};
  rofi-themes = fetchFromGitHub {
    owner = "DaveDavenport";
    repo = "rofi-themes";
    rev = "c16c7e91a313e4f325d631832381f628778feea1";
    sha256 = "1d9h24gyi16gjp4kvvmmdqnbfw3b0zy41z233gnrd20i64flqdl2";
  };
  nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") { inherit pkgs; };
  darkreader-extension = nur.repos.rycee.firefox-addons.buildFirefoxXpiAddon {
    pname = "dark-reader";
    version = "4.9.33";
    addonId = "addon@darkreader.org";
    url = "https://addons.mozilla.org/firefox/downloads/file/3783471/dark_reader-4.9.33-an+fx.xpi";
    sha256 = "0n9i5vdmjbzhvyvx0mircajrbs484xc4492sg3xkv5fvrw9yrrs9";
    meta = {};
  };
  brotab-extension = nur.repos.rycee.firefox-addons.buildFirefoxXpiAddon {
    pname = "brotab";
    version = "1.3.0";
    addonId = "brotab_mediator@example.org";
    url = "https://addons.mozilla.org/firefox/downloads/file/3583505/brotab-1.3.0-an+fx.xpi";
    sha256 = "187frmvb6z7k3p4yr15v10y4piz0fm6zhqp1jxqqhyaxdszrzwqw";
    meta = {};
  };
  emax = import ./packages/emax { inherit pkgs; };
  firefox-beta-with-extensions =
    (wrapFirefox latest.firefox-beta-bin {}).override({browserName = "firefox";});
#         .overrideAttrs(oldAttrs:
#       { browserName = "firefox";
#         postBuild = # oldAttrs.postBuild +
#           ''
#             #TODO : This doesnt work when used inside home manager
#             #ln -sLt $out/lib/mozilla/native-messaging-hosts ${brotab}/lib/python3.8/site-packages/brotab/mediator/firefox_mediator.json
#           '';
#       });
  tomb-overlay = import ../common/pkgs/tomb.nix;
  tmuxWithConfig = callPackage ./packages/tmux/default.nix {};
in
rec {
  news.display = "silent";
  manual = {
    html.enable = true;
    json.enable = true;
  };
  nixpkgs.config = {
    keep-derivations = true;
    keep-outputs = true;
    allowUnfree = true;
    vivaldi = {
      proprietaryCodecs = true;
      enableWideVine = true;
    };
  };
  nixpkgs.overlays = [
    xdotool-overlay
    tomb-overlay
    brotab-overlay
    ripgrep-overlay
    neovim-overlay
    rofi-fuzzy
    pass-override-overlay
    pass-extensions-overlay
    ffmpeg-overlay
    mpd-overlay
    firefox-overlay
    julia-overlay
    urxvt-perls
    electron-apps
    wallpaper-overlay
    compton-git-overlay
    surfraw-overlay ];
  services.dunst = {
    enable = true;
    settings = import ./config/dunstrc.nix;
    iconTheme = {
      name = "Numix";
      package = numix-icon-theme;
      size = "48";
    };
  };
#   services.polybar = {
#     enable = true;
#     package =
#       polybar.override {
#         alsaSupport = true;
#         i3GapsSupport = true;
#         iwSupport = true;
#         nlSupport = false;
#       };
#     extraConfig = import ./config/polybar/polybarrc.nix { inherit pkgs config; };
#     script = builtins.readFile "${polybarLaunch}/bin/launchPolybar";
#   };
  xsession = {
    enable = true;
    initExtra = lib.readFile onAttachMonitor;
    windowManager.command = "${import ./scripts/mk-stumpwm.nix { inherit pkgs;}}";
#     windowManager.command = "${sbcl}/bin/sbcl --load /home/viktor/stuff/stumpwm-config/init.lisp";
#     windowManager.command = "${stumpwm}/bin/stumpwm";
#     windowManager.i3 = import ./config/i3/i3config.nix { inherit pkgs rofiElectronAppsRunner shareLink ; };
  };
  imports = [
    # ./services/pCloudCC.nix
    ./services/mpd.nix
  ];
  # Let Home Manager install and manage itself.
  #programs.home-manager.enable = true;
  programs.zsh = import ./config/zsh.nix { inherit pkgs config tmuxWithConfig; };
  fonts.fontconfig.enable = true;
#  services.pCloudCC = {
#    enable = true;
#    loginId = "paulvictor@gmail.com";
#    mountPoint = "${config.home.homeDirectory}/pcloud";
#    package = pCloudCC;
#  };
#  systemd.user.paths.MountMusic = {
#    Unit = {
#      Description = "Bind mount Music from pCloud";
#      After = [ "pCloudCC.service" ];
#    };
#    Path = {
#      PathExists = "${config.home.homeDirectory}/pcloud/Music";
#    };
#  };
#  systemd.user.services.MountMusic = {
#    Install = {
#        WantedBy = [ "default.target" ];
#    };
#    Unit = {
#      Description = "Mount Music Directory";
#      After = [ "pCloudCC.service" ];
#    };
#    Service = {
#      ExecStart = "${bindfs}/bin/bindfs -d ${config.home.homeDirectory}/pcloud/Music ${config.home.homeDirectory}/Music";
#      Type = "simple";
#    };
#  };
#   systemd.user.services.xmodmap = {
#     Unit = {
#       Description = "Loads the XModMap Keymap";
#       After = [ "graphical-session.target" ];
#       Requires = [ "graphical-session.target" ];
#       # Wants = [ "display-manager.service" ];
#     };
#     Service = {
#       Environment="XAUTHORITY=${config.home.homeDirectory}/.Xauthority";
#       ExecStart = "${xorg.xmodmap}/bin/xmodmap -verbose ${config.home.homeDirectory}/.Xmodmap";
#       Type = "oneshot";
#       RemainAfterExit = "yes";
#     };
#   };
  home.packages = [
    acpi # TODO : Install only on laptops
    afuse
    ag
    agenix.agenix
    android-file-transfer
    android-studio
    arandr
    asciinema
    autorandr
    axel
    bat
    bind
    bindfs
    brotab
    cantarell-fonts
    #cachix
    comma
    emax.desktopApp
    emax.cliApp
    ungoogled-chromium
    ddgr
    deluge
    dejavu_fonts
    discord
    dunst
    #dyalog-nixos.dyalog #dyalog-nixos.ride
    electronApps
    enscript
    entr
    exa
    feh
    ferdi
    ffmpeg-full
    findWindowByTitle
    fira-code
    firacode-nerdfonts
    font-awesome_5
    font-awesome-ttf
    fzf
    google-cloud-sdk
    google-chrome
    googler
    google-play-music-desktop-player
    gnome3.defaultIconTheme
    gnome3.networkmanagerapplet
    gnumake
    gromit-mpx
    hack-nerdfonts
    haskellPackages.niv
    hicolor_icon_theme
    (iosevka-bin.override { variant = "aile"; })
    (iosevka-bin.override { variant = "slab"; })
    i3exit
    # jailed-firefox
    jq
    keybase
    kubectl
    kubectx
    kube-prompt
    kubetail
    k9s
    league-of-moveable-type
    material-icons
    menu-surfraw
    moreutils
    mpc_cli
    mpv
    msgpack-tools
    ncmpcpp
    nix-bundle
    nix-index
    nixos-generators
    nodejs-10_x
    nodePackages.bower
    nodePackages.bower2nix
    nodePackages.node2nix
    noto-fonts
    opensc
    openssh
    openssl
    pa_applet
    paprefs
    #pass-with-extensions
    passdo
    pasystray
    pavucontrol
    #pCloudCC
    pdftk
    ((pinentry.override({ enabledFlavors = ["curses"];})).overrideAttrs(oldAttrs: {
      buildInputs = oldAttrs.buildInputs ++ [ git ];
      #postInstall = "unlink $out/bin/pinentry";
      postInstall = "";
      outputs = [ "out" ];
    }))
    lsof
    pbgopy
    pcsclite
    pcsctools
    pms
    prettyping
    #pscid
    pulseaudio-ctl
    pursuit
    pv
    qrcp
    quickswitch-for-i3
    rclone
    rclone-browser
    rofi
    rofiElectronAppsRunner
    rofi-systemd
    ruby
    rxvt_unicode-with-plugins
    scrot
    shareLink
    siji
    skim
    source-code-pro-nerdfonts
    stow
    surf-webkit2
    surfraw
    #texlive.combined.scheme-full
    tmate
    tmuxWithConfig
    tomb
    tree
    ubuntu_font_family
    unifont
    unzip
    #victor-mono
    victor-mono-nerdfonts
    all-the-icons-fonts
    vifm
    vlc
    vlc_qt5
    (vivaldi.override { proprietaryCodecs = true; inherit vivaldi-ffmpeg-codecs;})
    watchexec
    weechat
    wmctrl
    wmfocus
    xclip
    xdotool
    xorg.xdpyinfo
    xorg.xmodmap
    xsel
    youtube-dl
    yubikey-manager
    yubikey-personalization
    yubico-piv-tool
    zathura
    zoom-us
    z-lua
  ] ++
  (with easy-purs; [ psc-package purescript spago purp ]);
  xresources.extraConfig = builtins.readFile (
    builtins.fetchurl {
      url = https://github.com/chriskempson/base16-xresources/raw/master/xresources/base16-solarized-dark-256.Xresources;
      sha256 = "14dzphqvw4djxy25y5yic87c1rmfc09kb7g5b2bp0y31jz18lg90";
  });
  xresources.properties = import ./config/Xresources;
  #services.kbfs = {
    #enable = true;
    #extraFlags = [ "-label kbfs" "-mount-type normal" ];
  #};
  #services.keybase.enable = true;
  #services.kdeconnect.enable = true;
  #services.kdeconnect.indicator = true;
  xdg.configFile."zsh/themes/spaceship.zsh-theme".source =
    let src = fetchFromGitHub {
      "owner" = "denysdovhan";
      "repo" = "spaceship-prompt";
      "rev" = "d9f25e14e7bec4bef223fd8a9151d40b8aa868fa";
      "sha256" = "0vl5dymd07mi42wgkh0c3q8vf76hls1759dirlh3ryrq6w9nrdbf";
    }; in "${src.out}/spaceship.zsh-theme";
  xdg.configFile."zsh/plugins/zsh-completions/zsh-completions.plugin.zsh".source =
    let src = fetchFromGitHub {
      "owner" = "zsh-users";
      "repo" = "zsh-completions";
      "rev" = "5dd73237d598563e03ea8e84ff9deb6a6ed70848";
      "sha256" = "1yf4rz99acdsiy0y1v3bm65xvs2m0sl92ysz0rnnrlbd5amn283l";
    }; in "${src.out}/zsh-completions.plugin.zsh";
  xdg.configFile."surfraw/conf".source = import ./config/surfraw { inherit pkgs; };
  xdg.configFile."surfraw/elvi/hoogle" = {
    text = let hoogle = import ./scripts/elvis.nix {
      inherit pkgs;
      name = "hoogle";
      searchUrl = "https://www.haskell.org/hoogle/?hoogle=";
      baseUrl = "https://www.haskell.org/hoogle/";
    };
    in lib.readFile "${hoogle}/bin/hoogle";
    executable = true;
  };
  xdg.configFile."surfraw/elvi/pursuit" = {
    text = let pursuit = import ./scripts/elvis.nix {
      inherit pkgs;
      name = "pursuit";
      searchUrl = "https://pursuit.purescript.org/search?q=";
      baseUrl = "https://pursuit.purescript.org/";
    };
    in lib.readFile "${pursuit}/bin/pursuit";
    executable = true;
  };
  xdg.configFile."rofi/material.rasi".source = "${rofi-themes}/User Themes/material.rasi";
  xdg.configFile."rofi/onedark.rasi".source = "${rofi-themes}/User Themes/onedark.rasi";
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
  xdg.configFile."kitty/kitty.conf".source = import ./config/kitty.nix { inherit pkgs; };
  xdg.configFile."pulse/default.pa".text = lib.readFile ./config/pulseaudio.conf;
  xdg.configFile."rofi/config.rasi".source = import ./config/rofi-config.nix {inherit pkgs; };
  xdg.configFile."zathura/zathurarc".text = ''
    set selection-clipboard clipboard
  '';
  xdg.configFile."nvim/coc-settings.json".text = ''
    "languageserver": {
      "haskell": {
        "command": "hie-wrapper",
        "rootPatterns": [
          ".stack.yaml",
          "cabal.config",
          "package.yaml"
        ],
        "filetypes": [
          "hs",
          "lhs",
          "haskell"
        ],
        "initializationOptions": {
          "languageServerHaskell": {
          }
        },
      }
    }
  '';
  home.file.".ghc/ghci.conf".text = ''
    :def hoogle \s -> return $ ":! hoogle search --color --count=15 \"" ++ s ++ "\""
    :def doc \s -> return $ ":! hoogle search --color --info \"" ++ s ++ "\""
  '';
  home.file.".gitconfig".text = builtins.readFile ./config/git-config;
  home.file.".Xmodmap".text =
    lib.concatStringsSep "\n"
    [ # (builtins.readFile ./config/apl.xmodmap)
      (builtins.readFile ./config/xmodmap)];
  programs.autorandr = {
    enable = true;
    hooks.postswitch = {
      "notify-i3" = "${i3-gaps}/bin/i3-msg restart";
      "reload-wp" = "${feh}/bin/feh --bg-scale ${wall1} ${wall2} ${wall3}";
      "reload-compton" = ''systemctl --user restart compton.service'';
      "restart-polybar" = "systemctl --user restart polybar.service";
      "restart-yubikey-touch-detector" = "systemctl --user restart yubikey-touch-detector.service";
    };
    profiles = {
      triple = {
        fingerprint = {
          DP1-8 = "00ffffffffffff0010acb9a04c4d45312f1b0104a53420783a0495a9554d9d26105054a54b00714f8180a940d1c0d100010101010101283c80a070b023403020360006442100001e000000ff00395433434d37424c31454d4c0a000000fc0044454c4c2055323431350a2020000000fd00313d1e5311000a20202020202001e802031cf14f9005040302071601141f12132021222309070783010000023a801871382d40582c450006442100001e011d8018711c1620582c250006442100009e011d007251d01e206e28550006442100001e8c0ad08a20e02d10103e96000644210000180000000000000000000000000000000000000000000000000000000c";
          HDMI1 = "00ffffffffffff0010acc2d0543130311b1c010380351e78eaad75a9544d9d260f5054a54b008100b300d100714fa9408180d1c00101565e00a0a0a02950302035000e282100001a000000ff003458305256383735313031540a000000fc0044454c4c205032343138440a20000000fd0031561d711c000a202020202020019102031bb15090050403020716010611121513141f2065030c001000023a801871382d40582c45000e282100001e011d8018711c1620582c25000e282100009ebf1600a08038134030203a000e282100001a7e3900a080381f4030203a000e282100001a00000000000000000000000000000000000000000000000000000000d8";
          DP1-1 = "00ffffffffffff0010acc1d0545742301b1c0104a5351e783aad75a9544d9d260f5054a54b008100b300d100714fa9408180d1c00101565e00a0a0a02950302035000e282100001a000000ff003458305256383735304257540a000000fc0044454c4c205032343138440a20000000fd0031561d711c010a20202020202001ad020315b15090050403020716010611121513141f20023a801871382d40582c45000e282100001e011d8018711c1620582c25000e282100009ebf1600a08038134030203a000e282100001a7e3900a080381f4030203a000e282100001a0000000000000000000000000000000000000000000000000000000000000000000062";
        };
        config = {
          "HDMI1" = {
            enable = true;
            mode = "2560x1440";
            position = "1200x560";
            rate = "60.0";
            rotate = "normal";
            primary = true;
          };
          "DP1-8" = {
            enable = true;
            mode = "1920x1200";
            position = "0x412";
            rate = "60.0";
            rotate = "right";
            primary = false;
          };
          "DP1-1" = {
            enable = true;
            mode = "2560x1440";
            position = "3760x0";
            rate = "60.0";
            rotate = "left";
            primary = false;
          };
        };
      };
      dual = {
        fingerprint = {
          DP1-8 = "00ffffffffffff0010acb9a04c4d45312f1b0104a53420783a0495a9554d9d26105054a54b00714f8180a940d1c0d100010101010101283c80a070b023403020360006442100001e000000ff00395433434d37424c31454d4c0a000000fc0044454c4c2055323431350a2020000000fd00313d1e5311000a20202020202001e802031cf14f9005040302071601141f12132021222309070783010000023a801871382d40582c450006442100001e011d8018711c1620582c250006442100009e011d007251d01e206e28550006442100001e8c0ad08a20e02d10103e96000644210000180000000000000000000000000000000000000000000000000000000c";
          HDMI1 = "00ffffffffffff0010ac604157313433181c010380351e78ea2585a9544ca2260a5054a54b00714f8180a9c0d1c00101010101010101023a801871382d40582c45000f282100001e000000ff00424d4d52354e320a2020202020000000fc0044454c4c205332343139484d0a000000fd00304b1e5311000a20202020202001b802032bf14f90050403020716010611121513141f23097f078301000065030c001000681a00000101304be62a4480a070382740302035000f282100001a011d8018711c1620582c25000f282100009e011d007251d01e206e2855000f282100001e8c0ad08a20e02d10103e96000f282100001800000000000000000000000034";
        };
        config = {
          "HDMI1" = {
            enable = true;
            mode = "1920x1080";
            position = "0x456";
            rate = "60.0";
            rotate = "normal";
            primary = true;
          };
          "DP1-8" = {
            enable = true;
            mode = "1920x1200";
            position = "1920x0";
            rate = "60.0";
            rotate = "right";
            primary = false;
          };
        };
      };
    };
  };
  services.picom = {
    enable = true;
    backend = "glx";
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
  #services.flameshot.enable = true;
  services.network-manager-applet.enable = true;
  services.pasystray.enable = true;
  programs.neovim.enable = true;# = import ./config/vim/hm.nix { inherit pkgs; };
  programs.firefox = {
    enable = true;
    #package = firefox-beta-bin ; # wrapFirefox (latest.firefox-beta-bin) { browserName = "firefox"; };
    extensions = [
      #nur.repos.rycee.firefox-addons.violentmonkey
      #nur.repos.rycee.firefox-addons.tree-style-tab
      #nur.repos.rycee.firefox-addons.temporary-containers
      #nur.repos.rycee.firefox-addons.refined-github
      #nur.repos.rycee.firefox-addons.i-dont-care-about-cookies
      #nur.repos.rycee.firefox-addons.https-everywhere
      #nur.repos.rycee.firefox-addons.google-search-link-fix
      nur.repos.rycee.firefox-addons.tridactyl
      brotab-extension
      darkreader-extension
      #nur.repos.rycee.firefox-addons.dark-night-mode
    ];
    profiles = {
      "proxied" = {
        id = 1;
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = false;
          "network.proxy.socks" = "localhost";
          "network.proxy.socks_port" = 6767;
          "network.proxy.socks_remote_dns" = true;
        };
      };
      "usual" = {
        id = 0;
        settings = {
          "browser.shell.checkDefaultBrowser" = false;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "network.http.pipelining" = true;
          "network.http.pipelining.maxrequests" = 16;
          "network.http.max-connections" = 1024;
          "network.http.max-connections-per-server" = 32;
          "nglayout.initialpaint.delay" = 0;
          "browser.startup.page" = 3; # restore previous session
          "privacy.resistFingerprinting.block_mozAddonManager" = true;
          "extensions.webextensions.restrictedDomains" = "";
          "browser.sessionStore.warnOnQuit" = false;
          "browser.warnOnQuit" = false;
          "signon.rememberSignons" = false;
          "layers.acceleration.force-enabled" = true;
          "gfx.webrender.all" = true;
          "svg.context-properties.content.enabled" = true;
          "full-screen-api.approval-required" = false;
          "full-screen-api.warning.delay" = 0;
          "full-screen-api.warning.timeout" = 0;
          #"full-screen-api.transition-duration.enter" = 0 0;
          #"full-screen-api.transition-duration.leave" = 0 0;
          "browser.tabs.loadBookmarksInTabs" = true;
          "browser.tabs.loadBookmarksInBackground" = true;
          "browser.tabs.tabMinWidth" = 50;
          "browser.urlbar.dnsResolveSingleWordsAfterSearch" = 0;
          "browser.link.open_newwindow.restriction" = 0;
          "privacy.popups.showBrowserMessage" = true;
          "extensions.pocket.enabled" = false;
          "extensions.screenshots.disabled" = true;
          "ui.prefersReducedMotion" = 1;
          "browser.compactmode.show" = true;
        };
        userChrome = import ./config/userChrome.nix { inherit pkgs; };
      };
    };
  };
  home.file.".mozilla/native-messaging-hosts/brotab_mediator.json".source =
    "${brotab.out}/config/firefox_mediator.json";
  home.file.".mozilla/native-messaging-hosts/tridactyl.json".source =
    "${tridactyl-native}/lib/mozilla/native-messaging-hosts/tridactyl.json";
  xdg.configFile."tridactyl/tridactylrc".text =
    import ./config/tridactylrc.nix { inherit pkgs; };
  xdg.configFile."tridactyl/themes".source = fetchFromGitHub {
    owner = "bezmi";
    repo = "base16-tridactyl";
    rev = "092a88c2233c10a1b28cad647b2bf3fd667aaa84";
    sha256 = "19mqavlz713ilr0x8a6j7205irns069nx0a85f4h7xs0pc2kknhz";
    fetchSubmodules = true;
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
           ++ [ "-- ${i3exit}/bin/i3exit lock" ]);
    };
  };

  programs.ssh = {
    enable = true;
    compression = true;
    controlMaster = "auto";
    controlPath = "/tmp/ssh_mux_%h_%p_%r";
    controlPersist = "24h";
    forwardAgent = true;
    hashKnownHosts = true;
    serverAliveInterval = 30;
    serverAliveCountMax = 5;
    extraConfig = ''
      IdentityFile ~/.ssh/id_rsa.pub
    '';
    matchBlocks = {
      "github" = {
        host = "github";
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_rsa.pub";
      };
      "bitbucket" = {
        host = "bitbucket";
        hostname = "bitbucket.org";
        user = "git";
        identityFile = "~/.ssh/id_rsa.pub";
      };
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableZshIntegration = true;
    stdlib = ''
      : ''${XDG_CACHE_HOME:=$HOME/.cache}
      declare -A direnv_layout_dirs
      direnv_layout_dir() {
        echo "''${direnv_layout_dirs[$PWD]:=$(
            echo -n "$XDG_CACHE_HOME"/direnv/layouts/
            echo -n "$PWD" | shasum | cut -d ' ' -f 1
        )}"
      }
    '';
  };

  programs.gpg = {
    enable = true;
    settings = {
      default-key = "0xA96C9B89755DF7D2";
      default-recipient-self = true;
      require-cross-certification = true;
      no-symkey-cache = true;
    };
    scdaemonSettings = {
      disable-ccid = true;
      pcsc-driver = "${pkgs.pcsclite.out}/lib/libpcsclite.so";
      card-timeout = "1";
    };
  };

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

  programs.password-store = {
    enable = true;
    package = pass-with-extensions;
    settings = {
      PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
      PASSWORD_STORE_KEY = "3E6925C73B18D3DB43A2104EA96C9B89755DF7D2";
      PASSWORD_STORE_X_SELECTION = "SECONDARY";
    };
  };

  services.mbsync = {
    enable = true;
    package = pkgs.writeShellScriptBin "mbsync" ''
       ${pkgs.isync}/bin/mbsync $@
       NOTMUCH_CONFIG=${config.xdg.configHome}/notmuch/notmuchrc ${pkgs.notmuch}/bin/notmuch new
    '';
  };
  programs.neomutt = {
    enable = true;
    checkStatsInterval = 20;
    sidebar = {
      enable = true;
    };
    sort = "threads";
    vimKeys = true;
    extraConfig = ''
      set sort_aux = reverse-last-date-received # Thread view
      set mark_old=no # new messages are always new untill they have been read.

      # Set preffered view modes
      auto_view text/html # text/plain text/calendar # view html automatically
      alternative_order text/plain text/enriched text/html
      source ${pkgs.neomutt}/share/doc/neomutt/colorschemes/solarized-dark-256.neomuttrc
      # Compose a new email (not a reply) to the sender
      bind index,pager @ compose-to-sender
      # We replace the date field '%{%b %d}', giving:
      set index_format='%4C %Z %<[y?%<[m?%<[d?%[%H:%M ]&%[%a %d]>&%[%b %d]>&%[%m/%y ]> %-15.15L (%?l?%4l&%4c?) %s'
      # Test  Date Range  Format String  Example
      # --------------------------------------------
      # %[d   Today       %[%H:%M ]      12:34
      # %[m   This month  %[%a %d]       Thu 12
      # %[y   This year   %[%b %d]       Dec 10
      #  —    Older       %[%m/%y ]      06/15

      set new_mail_command = "${pkgs.libnotify}/bin/notify-send --icon='${pkgs.neomutt}/share/doc/neomutt/logo/neomutt.svg' 'New Email' '%n new messages, %u unread.' &"

      bind index,pager <f1> sidebar-prev      # Previous Mailbox
      bind index,pager <f2> sidebar-next      # Next Mailbox
      bind index,pager \CP sidebar-prev       # Ctrl-Shift-P - Previous Mailbox
      bind index,pager \CN sidebar-next       # Ctrl-Shift-N - Next Mailbox
      bind index,pager \CO sidebar-open       # Ctrl-Shift-O - Open Highlighted Mailbox
      bind index,pager B sidebar-toggle-visible   # Use 'B' to switch the Sidebar on and off
      bind pager a group-reply                # Reply to all recipients
      bind attach <enter>    view-mailcap

      set edit_headers                     # show headers when composing
      set fast_reply                       # skip to compose when replying
      set askcc                            # ask for CC:
      set fcc_attach                       # save attachments with the body
      set forward_format = "Fwd: %s"       # format of subject when forwarding
      set forward_decode                   # decode when forwarding
      set attribution = "On %d, %n wrote:" # format of quoting header
      set reply_to                         # reply to Reply to: field
      set reverse_name                     # reply as whomever it was to
      set include                          # include message in replies
      set forward_quote                    # include message in forwards
      set text_flowed
      unset sig_dashes                     # no dashes before sig
      unset mime_forward                   # forward attachments as part of body

      unset confirmappend      # don't ask, just do!
      set quit                 # don't ask, just do!!
      unset mark_old           # read/new is good enough for me
      set beep_new             # bell on new mails
      set pipe_decode          # strip headers and eval mimes when piping
      set thorough_search      # strip headers and eval mimes before searching
      set timeout = 0

      set uncollapse_jump
      set sort_re
      set reply_regexp = "^(([Rr][Ee]?(\[[0-9]+\])?: *)?(\[[^]]+\] *)?)*"
      set quote_regexp = "^( {0,4}[>|:#%]| {0,4}[a-z0-9]+[>|]+)+"
      set send_charset = "utf-8:iso-8859-1:us-ascii"
      set charset = "utf-8"
      set arrow_cursor = "no" # Change `color indicator` depending
      set query_command="${pkgs.notmuch-addrlookup}/bin/notmuch-addrlookup --mutt '%s'"

      macro index <F5> \
        "<enter-command>set my_old_pipe_decode=\$pipe_decode my_old_wait_key=\$wait_key nopipe_decode nowait_key<enter>\
        <shell-escape>${pkgs.notmuch-mutt}/bin/notmuch-mutt -r --prompt search<enter>\
        <change-folder-readonly>`echo ''${XDG_CACHE_HOME:-$HOME/.cache}/notmuch/mutt/results`<enter>\
        <enter-command>set pipe_decode=\$my_old_pipe_decode wait_key=\$my_old_wait_key<enter>" \
              "notmuch: search mail"

      macro index \\\\ "<vfolder-from-query>" "Search mailbox"
      macro index A "<modify-labels>+archive -unread -inbox<enter>" "Archive message"
      macro index c "<change-vfolder>?" "Change to vfolder overview"
      macro index,pager gi "<change-folder>=INBOX<enter>"       "open inbox"
      macro index,pager gd "<change-folder>=Drafts<enter>"      "open drafts"
      macro index,pager gs "<change-folder>=Sent<enter>"        "open sent"
      macro index,pager V "<pipe-message>${extract_url}/bin/extract_url<enter> " "Get URLs from email"
    '';
  };
  programs.mbsync = {
    enable = true;
  };
  programs.notmuch = {
    enable = true;
    new.tags = [ "new" "unread" ];
    hooks.postNew =
      let
        notmuch-rules = pkgs.writeText "tagmails.nm" ''
          -new path:/Trash/
          -new path:/Sent/
          -new path:/Spam/
          +inbox path:/INBOX/
          +sent  from:paul.victor@juspay.in
          +to-me to:paul.victor@juspay.in and tag:new
          +meetings subject:(Invitation: paul.victor@juspay.in) and tag:new
          +meetings subject:(Canceled event paul.victor@juspay.in) and tag:new
          +docs subject:"Invitation to edit" and tag:new
          +ignored -new -inbox from:support@juspay.in and tag:new
          +ignored -new -inbox from:no-reply@juspay.in and tag:new
          +ignored -new -inbox to:analytics@juspay.in and tag:new
          +ignored -new -inbox to:monitor@juspay.in and tag:new
          +ooo -new -inbox subject:/OOO/ and tag:new
          -new tag:new
        '';
      in ''
        # Run on first import
        # notmuch tag -unread -- tag:unread -- to mark existing mails as not unread
        ${notmuch}/bin/notmuch tag --batch --input=${notmuch-rules}
      '';
  };
  programs.alot = {
    enable = true;
  };
  programs.msmtp.enable = true;
  programs.astroid = {
    enable = true;
    externalEditor = "nvim  -- -c 'set ft=mail' '+set fileencoding=utf-8' '+set ff=unix' '+set enc=utf-8' '+set fo+=w' %1";
    extraConfig = {
      "astroid.hints.level" = -1;
    };
  };
  accounts.email.accounts = rec {
    "juspay.in" = {
      primary = true;
      #primary = false;
#       passwordCommand = "${pkgs.gnupg}/bin/gpg -dq .password-store/Juspay/Email/Mail/AllSpark-Dell.gpg"; # TODO Use address
      neomutt = {
        enable = true;
        extraConfig = ''
          set pgp_sign_as = 0xBFE9590DD8D67680
          set virtual_spoolfile=yes
          virtual-mailboxes \
            "To Me" "notmuch://?query=tag:to-me and not tag:ignored" \
            "Recent" "notmuch://?query=date:today or date:yesterday and not tag:ignored" \
            "Unread" "notmuch://?query=tag:unread and not tag:ignored" \
            "Sent" "notmuch://?query=tag:sent" \
            "Meetings" "notmuch://?query=tag:meetings" \
            "Docs" "notmuch://?query=tag:docs" \
            "OOO" "notmuch://?query=tag:ooo"
          set nm_exclude_tags = "junk-internal,analytics,deleted"
          set nm_unread_tag = unread
          '';
      };
      msmtp.enable = true;
      address = "paul.victor@juspay.in";
      realName = "Paul Victor Raj";
      flavor = "gmail.com";
      mbsync = {
        enable = false;
        create = "both";
        expunge = "both";
        extraConfig.account = {
          PipelineDepth = 50;
          Timeout = 60;
        };
        extraConfig.local = {
          MaxSize = "500k";
        };
        groups.main.channels = {
          inbox.extraConfig = {
            #MaxMessages = 2000;
            Patterns = "\"INBOX\"";
            ExpireUnread = "no";
          };
          drafts = {
            #masterPattern = "[Gmail]/Drafts";
            slavePattern = "Drafts";
            extraConfig = {
              Create = "Both";
              Expunge = "Both";
            };
          };
          sent = {
            extraConfig = {
              #MaxMessages = 2000;
              Create = "Both";
              Expunge = "Both";
            };
            #masterPattern = "[Gmail]/Sent Mail";
            slavePattern = "Sent";
          };
          starred= {
            #masterPattern = "\"[Gmail]/Starred\"";
            slavePattern = "Starred";
            extraConfig = {
              Create = "Both";
              Expunge = "Both";
            };
          };
        };
      };
      imap = {
        host = "imap.gmail.com";
        port = 993;
        tls = {
          enable = true;
        };
      };
      folders = {
        inbox = "INBOX";
        sent = "Sent";
        drafts = "Drafts";
        trash = "Trash";
      };
      signature = {
        showSignature = "append";
        text = ''
          Paul Victor Raj ( 0x18497AC961BB2FB6 )
          Sent from ${pkgs.neomutt}/bin/neomutt
        '';
      };
      notmuch.enable = true;
      imapnotify = {
        enable = true;
        boxes = [ "INBOX" ];
      };
      astroid = {
        enable = true;
      };
      gpg = {
        key = "3E6925C73B18D3DB43A2104EA96C9B89755DF7D2";
        signByDefault = true;
      };
    };
  };
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
  systemd.user.timers.batteryAlert = {
    Unit.Description = "Check if battery level has to be warned";
    Unit.Requires = "batteryAlert.service";
    Timer.OnCalendar="*-*-* *:*:00";
    Install.WantedBy = [ "timers.target" ];
  };
  systemd.user.services.batteryAlert = {
    Install.WantedBy = [ "default.target" ];
    Unit.Description = "Notify if battery is too low";
    Service = {
      ExecStart = "${batteryWarn}";
      Type = "simple";
    };
  };
  home.file.".mime.types".text = ''
    application/postscript          ps eps
    application/pgp                 pgp
    audio/x-aiff                    aif aifc aiff
    text/html                       html htm shtml
    image/gif                       gif;
    image/jpeg                      jpeg jpg;
  '';
  home.file.".mailcap".text = ''
    text/html; ${pkgs.w3m}/bin/w3m -I %{charset} -T text/html; copiousoutput; nametemplate=%s.html
    #text/html; ${pkgs.lynx}/bin/lynx -dump %s ; copiousoutput; nametemplate=%s.html; test=test -n "$DISPLAY"; needsterminal;
  '';
  programs.broot = {
    enable = true;
    enableZshIntegration = true;
    skin = {
      default = "gray(23) none / gray(20) none";
    };
  };
  home.file.".urlview".text = ''
    SHORTCUT
    NOREVIEW
    RAW_RESERVED
  '';
    #COMMAND ${firefox-beta-with-extensions}/bin/firefox %s &
  systemd.user.sessionVariables = {
    GNUPGHOME = "${config.home.homeDirectory}/.gnupg";
  };
  programs.exa = {
    enable = true;
    enableAliases = true;
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
      background_opacity = 1.0;
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
  services.sxhkd = {
    enable = true;
    keybindings = {
      "control + hyper + alt + Return" = "${alacritty}/bin/alacritty";
      "control + hyper + alt + shift + Return" = "${alacritty}/bin/alacritty -e tmux";
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
      "XF86Audio{Raise,Lower}Volume" = "pactl set-sink-volume @DEFAULT_SINK@ {+5%,-5%}"; #increase sound volume
      "XF86AudioMute" =  "pactl set-sink-mute  @DEFAULT_SINK@ toggle"; # mute sound
      # "XF86MicMute" =  "pulseaudio-ctl mute-input"; # mute mic
      # Sreen brightness controls
      "XF86MonBrightness{Up,Down}" = "light -{A,U} 5";
    };
  };
}
