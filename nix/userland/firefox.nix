{ config, pkgs, lib, specialArgs, ... }:

with pkgs;
let
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
    version = "1.24.4pre7194";
    addonId = "tridactyl.vim.betas@cmcaine.co.uk";
    url = "https://tridactyl.cmcaine.co.uk/betas/tridactyl2-1.24.3pre7189.xpi";
    sha256 = "sha256-FMOvkEe5ruFLz8jUllitDrdPJMszgRvKez8i61CMB/w=";
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
  commonFFProfile = {
    settings = {
      "identity.fxaccounts.enabled" = false;
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
      "layout.css.prefers-color-scheme.content-override" = 0;
      "ui.systemUsesDarkTheme" = 1;
    };
    userChrome = import ./config/userChrome.nix { inherit pkgs; };
    extensions.packages = [
      nur.repos.rycee.firefox-addons.i-dont-care-about-cookies
      #             nur.repos.rycee.firefox-addons.bypass-paywalls-clean
      brotab-extension
      edit-with-emacs-extension
      tabfs
      tridactyl
    ];
  };
in
{
  xdg.desktopEntries = {
    firefox-proxied = {
      name = "Firefox (Proxied)";
      genericName = "Web Browser";
      exec = "${config.programs.firefox.package}/bin/${config.programs.firefox.package.meta.mainProgram} -P proxied --no-remote %U";
      icon = "firefox-developer-edition";
      categories = [ "Network" "WebBrowser" ];
      terminal = false;
    };
    firefox-default = {
      name = "Firefox";
      genericName = "Web Browser";
      exec = "${config.programs.firefox.package}/bin/${config.programs.firefox.package.meta.mainProgram} -P dev-edition-default --no-remote %U";
      icon = "firefox-developer-edition";
      categories = [ "Network" "WebBrowser" ];
      terminal = false;
    };
  };

  programs.firefox = {
    enable = true;
    package = firefox-devedition;
    profiles = {
      "proxied" = {
        id = 1;
        settings = commonFFProfile.settings // {
          "network.proxy.type" = 1;
          "network.proxy.socks" = "gp-tunnel-host";
          "network.proxy.socks_port" = 1080;
          "network.proxy.socks_version" = 5;
          "network.proxy.socks_remote_dns" = true;
        };
        userChrome = commonFFProfile.userChrome + ''
          #TabsToolbar {
            background-color: #6b0000 !important;
          }
          .tab-background:is([selected], [multiselected]) {
            background-color: #3a0000 !important;
          }
        '';
        extensions.packages = commonFFProfile.extensions.packages;
      };
      "dev-edition-default" = {
        id = 0;
        path = "usual";
      };
      "usual" = {
        id = 2;
        settings = commonFFProfile.settings // {
          "browser.tabs.unloadOnLowMemory" = true;
          "identity.fxaccounts.enabled" = false;
          "browser.newtabpage.activity-stream.feeds.telemetry" = false;
          "browser.tabs.min_inactive_duration_before_unload" = 1800000;
          "browser.low_commit_space_threshold_mb" = 1024*1.5;
          "browser.cache.memory.capacity" = 4096;
          "dom.worker.throttle_inactive_tabs" = true;
          "browser.cache.disk.enable" = false;
          "browser.sessionstore.restore_on_demand" = true;
          "browser.tabs.loadBookmarksInBackground" = false;
          "browser.tabs.loadBookmarksInTabs" = false;
          "browser.tabs.loadInBackground" = false;
          "browser.ai.control.default" = "blocked";
          "layout.css.devPixelsPerPx" =
            if (builtins.elem specialArgs.hostname ["anarki" "slash"]) then "1.50" else "1.15";
        };
        userChrome = commonFFProfile.userChrome;
        extensions.packages = commonFFProfile.extensions.packages;
      };
    };
  };

  home.file.".mozilla/native-messaging-hosts/brotab_mediator.json".source =
    "${brotab.out}/config/firefox_mediator.json";
  home.file.".mozilla/native-messaging-hosts/tridactyl.json".source =
    "${tridactyl-native}/lib/mozilla/native-messaging-hosts/tridactyl.json";
  home.file.".mozilla/native-messaging-hosts/com.paulvictor.tabfs.json".text = tabfs-native-manifest;

  xdg.configFile."tridactyl/tridactylrc".text =
    import ./config/tridactylrc.nix { inherit pkgs; };
  xdg.configFile."tridactyl/themes".source = fetchFromGitHub {
    owner = "bezmi";
    repo = "base16-tridactyl";
    rev = "092a88c2233c10a1b28cad647b2bf3fd667aaa84";
    sha256 = "19mqavlz713ilr0x8a6j7205irns069nx0a85f4h7xs0pc2kknhz";
    fetchSubmodules = true;
  };
}
