{pkgs, config, ... }:

let
  mozilla-pkgs = import (fetchTarball https://github.com/mozilla/nixpkgs-mozilla/tarball/507efc7f62427ded829b770a06dd0e30db0a24fe);
  urxvt-perls = import ./packages/urxvt-perls/overlay.nix;
  electron-apps = import ./packages/electronApps;
  surfraw-overlay = import ./packages/surfraw.nix;
  mpd-overlay = import ./packages/mpd.nix;
  ffmpeg-overlay = import ./packages/ffmpeg.nix;
  hie = import ./packages/hie.nix { inherit pkgs; } ;
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};

in
with pkgs;
let
  pCloudCC =
    callPackage ./packages/pCloudCC/pCloudCC.nix {};
  easy-purs =
    import (fetchTarball https://github.com/justinwoo/easy-purescript-nix/tarball/a9be7dcde4785063d9f8dab2be646d6e9905ab66);
  pscid =
    callPackage ./packages/pscid/default.nix {
      inherit pkgs system;
      purescript = easy-purs.purescript;
    };
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
  brotab =
    callPackage ./packages/brotab {};
  pursuit =
    import ./scripts/pursuit.nix { inherit pkgs; };
  findWindowByTitle =
    import ./scripts/findWindowByTitle.nix { inherit pkgs; };
  batteryWarn =
    import ./scripts/batteryWarn.nix { inherit pkgs; };
  source-code-pro-nerdfonts =
    callPackage ./packages/source-code-pro-nerdfonts {};
  hack-nerdfonts =
    callPackage ./packages/hack-nerdfonts.nix {};
  firacode-nerdfonts =
    callPackage ./packages/fira-code-nerdfonts.nix {};
  menu-surfraw =
    import ./scripts/menu-surfraw.nix { inherit pkgs; };
  shareLink =
    import ./scripts/shareLink.nix { inherit pkgs config; };
in
rec {
  nixpkgs.overlays = [
    ffmpeg-overlay
    mpd-overlay
    mozilla-pkgs
    urxvt-perls
    electron-apps
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
  services.polybar = {
    enable = true;
    package =
      polybar.override {
        alsaSupport = true;
        i3GapsSupport = true;
        iwSupport = true;
        nlSupport = false;
      };
    config = ./config/polybar/polybarrc;
    script = builtins.readFile "${polybarLaunch}/bin/launchPolybar";
  };
  xsession = {
    enable = true;
    initExtra = lib.readFile onAttachMonitor;
    windowManager.i3 = import ./config/i3/i3config.nix { inherit pkgs rofiElectronAppsRunner shareLink ; };
  };
  imports = [
    ./services/pCloudCC.nix
    ./services/mpd.nix
  ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.neovim = import ./config/vim { inherit pkgs; };
  programs.zsh = import ./config/zsh.nix { inherit pkgs; };
  fonts.fontconfig.enableProfileFonts = true;
  services.pCloudCC = {
    enable = true;
    loginId = "paulvictor@gmail.com";
    mountPoint = "${config.home.homeDirectory}/pcloud";
    package = pCloudCC;
  };
  systemd.user.paths.MountMusic = {
    Unit = {
      Description = "Bind mount Music from pCloud";
      After = [ "pCloudCC.service" ];
    };
    Path = {
      PathExists = "${config.home.homeDirectory}/pcloud/Music";
    };
  };
  systemd.user.services.MountMusic = {
    Install = {
        WantedBy = [ "default.target" ];
    };
    Unit = {
      Description = "Mount Music Directory";
      After = [ "pCloudCC.service" ];
    };
    Service = {
      ExecStart = "${bindfs}/bin/bindfs -d ${config.home.homeDirectory}/pcloud/Music ${config.home.homeDirectory}/Music";
      Type = "simple";
    };
  };
  home.packages = [
    acpi # TODO : Install only on laptops
    ag
    axel
    bat
    bind
    bindfs
    brotab
    cantarell-fonts
    chromium
    ddgr
    dejavu_fonts
    dhall
    dhall-json
    dunst
    electronApps
    enscript
    entr
    feh
    ffmpeg-full
    findWindowByTitle
    fira-code
    firacode-nerdfonts
    font-awesome_5
    font-awesome-ttf
    fzf
    googler
    gimp-with-plugins
    gnome3.defaultIconTheme
    gnome3.networkmanagerapplet
    gnumake
    hack-nerdfonts
    #haskellPackages.hdevtools
    hicolor_icon_theme
    # Try using stack's or cabal's ability to build a static executable
    #hie.hies # has collision with conduit while developing
    #all-hies.versions.ghc864
    i3exit
    jq
    keybase
    kitty
    latest.firefox-beta-bin
    league-of-moveable-type
    material-icons
    menu-surfraw
    mpc_cli
    mpv
    ncmpcpp
    nodejs-10_x
    nodePackages_10_x.bower
    nodePackages_10_x.bower2nix
    nodePackages_10_x.pulp
    nodePackages.node2nix
    noto-fonts
    pa_applet
    paprefs
    pass
    passdo
    pasystray
    pavucontrol
    pCloudCC
    pdftk
    prettyping
    pscid
    pulseaudio-ctl
    pursuit
    quickswitch-for-i3
    rofi
    rofiElectronAppsRunner
    rofi-systemd
    ruby
    rxvt_unicode-with-plugins
    scrot
    shareLink
    skim
    source-code-pro-nerdfonts
    stow
    surf-webkit2
    surfraw
    termite
    texlive.combined.scheme-full
    tmux
    ubuntu_font_family
    unifont
    unzip
    vifm
    (vivaldi.override { proprietaryCodecs = true; inherit vivaldi-ffmpeg-codecs;})
    watchexec
    weechat
    wmctrl
    wmfocus
    xclip
    xdotool
    xsel
    youtube-dl
    zathura
    vscode
  ] ++
  (with easy-purs; [
    psc-package
    purescript
    spago
    purp
  ]);
  xresources.extraConfig = builtins.readFile (
    builtins.fetchurl {
      url = https://github.com/chriskempson/base16-xresources/raw/master/xresources/base16-solarized-dark-256.Xresources;
      sha256 = "14dzphqvw4djxy25y5yic87c1rmfc09kb7g5b2bp0y31jz18lg90";
  });
  xresources.properties = import ./config/Xresources;
  services.kbfs = {
    enable = true;
    extraFlags = [ "-label kbfs" "-mount-type normal" ];
  };
  services.keybase.enable = true;
  services.kdeconnect.enable = true;
  services.kdeconnect.indicator = true;
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
  xdg.configFile."rofi/material.rasi".source = fetchurl {
    url = "https://github.com/DaveDavenport/rofi-themes/raw/master/User%20Themes/material.rasi";
    sha256 = "09c9mni7g69lasxdlnv1wwwbr39ib666aafay642fzfqa9mb0ckp";
  };
  xdg.configFile."rofi/onedark.rasi".source = fetchurl {
    url = "https://github.com/DaveDavenport/rofi-themes/raw/master/User%20Themes/onedark.rasi";
    sha256 = "06qb8dr7b1zdxsx6vfakwrl0kba7ynnih6axw90f247yyvwwwbpq";
  };
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
  xdg.configFile."kitty/kitty.conf".source = import ./config/kitty.nix { inherit pkgs; };
  home.file.".ghc/ghci.conf".text = ''
    :def hoogle \s -> return $ ":! hoogle search --color --count=15 \"" ++ s ++ "\""
    :def doc \s -> return $ ":! hoogle search --color --info \"" ++ s ++ "\""
  '';
  home.file.".Xmodmap".text = ''
    clear Lock
    keycode 66 = Hyper_L
    add mod4 = Hyper_L
    pointer = 1 2 3 5 4 7 6 8 9 10 11 12
    keycode 232 = XF86MonBrightnessDown
    keycode 233 = XF86MonBrightnessUp

    keycode 121 = XF86AudioMute
    keycode 123 = XF86AudioRaiseVolume
    keycode 122 = XF86AudioLowerVolume

    keycode 173 = XF86AudioPrev
    keycode 172 = XF86AudioPlay
    keycode 171 = XF86AudioNext
    '';
}
