{ config, pkgs, lib, ... }:

with pkgs;
let
  custom-vieb = import ./packages/vieb.nix { inherit pkgs; inherit (config.home) homeDirectory; };
  source-code-pro-nerdfonts = pkgs.callPackage ./packages/source-code-pro-nerdfonts {};
  hack-nerdfonts = pkgs.callPackage ./packages/hack-nerdfonts.nix {};
#       firacode-nerdfonts = pkgs.callPackage ./packages/fira-code-nerdfonts.nix {};
  all-the-icons-fonts = pkgs.callPackage ./packages/all-the-icons-fonts.nix {};
  pursuit = pkgs.callPackage ./scripts/pursuit.nix {};
  menu-surfraw = pkgs.callPackage ./scripts/menu-surfraw.nix {};
  darkreader-extension = pkgs.nur.repos.rycee.firefox-addons.buildFirefoxXpiAddon {
    pname = "dark-reader";
    version = "4.9.33";
    addonId = "addon@darkreader.org";
    url = "https://addons.mozilla.org/firefox/downloads/file/3783471/dark_reader-4.9.33-an+fx.xpi";
    sha256 = "0n9i5vdmjbzhvyvx0mircajrbs484xc4492sg3xkv5fvrw9yrrs9";
    meta = {};
  };
  brotab-extension = pkgs.nur.repos.rycee.firefox-addons.buildFirefoxXpiAddon {
    pname = "brotab";
    version = "1.3.0";
    addonId = "brotab_mediator@example.org";
    url = "https://addons.mozilla.org/firefox/downloads/file/3583505/brotab-1.3.0-an+fx.xpi";
    sha256 = "187frmvb6z7k3p4yr15v10y4piz0fm6zhqp1jxqqhyaxdszrzwqw";
    meta = {};
  };
  edit-with-emacs-extension = pkgs.nur.repos.rycee.firefox-addons.buildFirefoxXpiAddon {
    pname = "edit-with-emacs-extension";
    version = "1.16";
    addonId = "{8dd384e7-fc9e-4b6a-a744-497edc3408c3}";
    url = "https://addons.mozilla.org/firefox/downloads/file/3708541/edit_with_emacs-1.16-an+fx.xpi";
    sha256 = "sha256-9nCmbDfhOfPUD+ljsOnXfU+ErJLgLx+XQ0SAojy4W5Q=";
    meta = {};
  };
  tridactyl = pkgs.nur.repos.rycee.firefox-addons.buildFirefoxXpiAddon {
    pname = "tridactyl";
    version = "1.23.0pre7038";
    addonId = "tridactyl.vim.betas@cmcaine.co.uk";
    url = "https://tridactyl.cmcaine.co.uk/betas/tridactyl2-1.24.1pre7038.xpi";
    sha256 = "070nrrgnz6v4hjdzgkvi31pnz79r8yn6cw1645f4p9f39ga945bb";
    meta = {};
  };
  tabfs = pkgs.nur.repos.rycee.firefox-addons.buildFirefoxXpiAddon {
    pname = "tabfs";
    version = "1.0";
    addonId = "tabfs@paulvictor.com";
    url = "https://github.com/paulvictor/TabFS/releases/download/c01449e/TabFS-1.0.xpi";
    sha256 = "sha256-23SwvUvX9d3/H+wwZoV17V6qgcORnvt0WEE+MORh/7Y=";
    meta = {};
  };
  tabfs-native-manifest =
    let
      tabfs-src = fetchFromGitHub {
        owner = "paulvictor";
        repo = "tabfs";
        rev = "c01449ee9b2e854b2f9ff214e4708ed7b63e2ef7";
        sha256 = "K2wD14EFZ7La6J+UGfL8+RRGGAUHhRfkHcZj+smhcd4=";
      };
      tabfs-fuse = stdenv.mkDerivation {
        name = "tabfs-fuse";
        src = tabfs-src;
        phases = [ "buildPhase" "installPhase" ];
        buildInputs = [ fuse ];
        nativeBuildInputs = [ makeWrapper ];
        buildPhase = ''
          mkdir -pv $out/bin
          $CC -O2 \
            -D_FILE_OFFSET_BITS=64 \
            -DFUSE_USE_VERSION=26 \
            -Wall \
            -Wextra \
            -Wno-unused-result \
            -g $NIX_CFLAGS_COMPILE \
            -o $out/bin/tabfs $src/fs/tabfs.c \
            -pthread -lfuse
        '';
        installPhase = ''
          wrapProgram $out/bin/tabfs \
          --prefix PATH : ${lib.makeBinPath [ procps gnugrep findutils ]} \
          --set TABFS_MOUNT_DIR ${config.home.homeDirectory}/tabs
        '';
      };
    in
      builtins.toJSON {
        name = "com.paulvictor.tabfs";
        description = "Tabfs";
        path = "${tabfs-fuse}/bin/tabfs";
        type = "stdio";
        allowed_extensions = ["tabfs@paulvictor.com"];
      };
in
  {
    home.packages =
      [
        all-the-icons-fonts
        autorandr
        brotab
        cantarell-fonts
        custom-vieb
        dejavu_fonts.full-ttf
        electronApps
        font-awesome

        googler
        gromit-mpx

        hicolor-icon-theme
        league-of-moveable-type
        localsend
        material-icons
        menu-surfraw
        mpv
        networkmanagerapplet
        noto-fonts
#             nyxt-3
        pdftk
        pursuit
        rofi
        rxvt-unicode
        scrot
        siji

        surf
        surfraw
        ubuntu_font_family
        unifont

        vlc
        yubico-piv-tool
        yubikey-manager
        yubikey-personalization
        (ungoogled-chromium.override { enableWideVine = pkgs.stdenv.isx86_64; })
        zathura # Crashing.

      ] ++
      (with nerd-fonts;
        [
          #hack
          victor-mono
          jetbrains-mono
          udev-gothic-nf
          plemoljp-nf
          #iosevka-term
          #iosevka
          sauce-code-pro
          #             dejavu-sans-mono
          symbols-only
          #fira-code
          monoid
        ])
      ++ (lib.optionals pkgs.stdenv.isx86_64
        [
          (vivaldi.override { proprietaryCodecs = true; enableWidevine = true;})

          google-chrome
        ]
      );
    xresources =
      let
        xresourcesFile = callPackage ./scripts/xresources.nix { template = "rxvt-unicode"; brightness = "dark"; scheme = "tomorrow"; };
      in {
        extraConfig =
          ''#define FONTSIZE 13'' + "\n" + builtins.readFile "${xresourcesFile}/config";
        properties = import ./config/Xresources/default.nix { inherit (pkgs) xclip; };
      };
    programs.firefox = {
      enable = true;
      package =
        if (pkgs.stdenv.isAarch64)
        then firefox-devedition
        else firefox-devedition-bin;
      #package = firefox-beta-bin ; # wrapFirefox (latest.firefox-beta-bin) { browserName = "firefox"; };
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
        "dev-edition-default" = {
          id = 0;
          path = "usual";
        };
        "usual" = {
          id = 2;
          settings = {
            "media.peerconnection.enabled" = true;
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
            "privacy.popups.showBrowserMessage" = false;
            "extensions.pocket.enabled" = false;
            "extensions.screenshots.disabled" = true;
            "extensions.autoDisableScopes" = 0;
            "ui.prefersReducedMotion" = 1;
            "browser.compactmode.show" = true;
            "xpinstall.signatures.required" = false;
          };
          userChrome = import ./config/userChrome.nix { inherit pkgs; };
          extensions = [
            nur.repos.rycee.firefox-addons.i-dont-care-about-cookies
#             nur.repos.rycee.firefox-addons.bypass-paywalls-clean
            brotab-extension
            edit-with-emacs-extension
            tabfs
            tridactyl
          ];
        };
      };
    };
    home.file.".mozilla/native-messaging-hosts/brotab_mediator.json".source =
      "${brotab.out}/config/firefox_mediator.json";
    home.file.".mozilla/native-messaging-hosts/tridactyl.json".source =
      "${tridactyl-native}/lib/mozilla/native-messaging-hosts/tridactyl.json";
    home.file.".mozilla/native-messaging-hosts/com.paulvictor.tabfs.json".text = tabfs-native-manifest;
    home.file.".vieb/colors/gruvbox.css".source = ./config/vieb/colors/gruvbox.css;
    home.file.".vieb/colors/smalltabs.css".source = ./config/vieb/colors/smalltabs.css;
    home.file.".vieb/colors/dark_minimal.css".source = ./config/vieb/colors/dark_minimal.css;
    xdg.configFile."tridactyl/tridactylrc".text =
      import ./config/tridactylrc.nix { inherit pkgs; };
    xdg.configFile."tridactyl/themes".source = fetchFromGitHub {
      owner = "bezmi";
      repo = "base16-tridactyl";
      rev = "092a88c2233c10a1b28cad647b2bf3fd667aaa84";
      sha256 = "19mqavlz713ilr0x8a6j7205irns069nx0a85f4h7xs0pc2kknhz";
      fetchSubmodules = true;
    };
    xdg.configFile = {
      "surfraw/conf" = {
        source = import ./config/surfraw { inherit pkgs; };
      };
      "surfraw/elvi/hoogle" = {
        text =
          let hoogle = import ./scripts/elvis.nix {
                inherit pkgs;
                name = "hoogle";
                searchUrl = "https://www.haskell.org/hoogle/?hoogle=";
                baseUrl = "https://www.haskell.org/hoogle/";
              };
          in lib.readFile "${hoogle}/bin/hoogle";
        executable = true;
      };
      "surfraw/elvi/pursuit" = {
        text =
          let pursuit = import ./scripts/elvis.nix {
                inherit pkgs;
                name = "pursuit";
                searchUrl = "https://pursuit.purescript.org/search?q=";
                baseUrl = "https://pursuit.purescript.org/";
              };
          in lib.readFile "${pursuit}/bin/pursuit";
        executable = true;
      };
    };
  }
