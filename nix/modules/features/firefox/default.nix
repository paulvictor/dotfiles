# Firefox Developer Edition with a plain and a proxied (GlobalProtect socks) profile, tridactyl, brotab
{
  allowedUnfree = [ "firefox-devedition-bin" "firefox-developer-edition-bin-unwrapped" ];

  flake.overlays.brotab = import ./_brotab-overlay.nix;

  flake.modules.homeManager.firefox = { lib, ... }: {
    imports = [ ./_firefox.nix ];

    options.custom.firefox.baseProfile = lib.mkOption {
      type = lib.types.deferredModule;
      description = "Settings, userChrome and extensions shared by my profiles; a profile pulls it in with `imports`";
    };

    options.custom.firefox.devPixelsPerPx = lib.mkOption {
      type = lib.types.str;
      default = "1.15";
      description = "layout.css.devPixelsPerPx of the main profile; set per host for HiDPI screens";
    };

    config.xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/http" = [ "firefox-devedition.desktop" ];
        "x-scheme-handler/https" = [ "firefox-devedition.desktop" ];
        "x-scheme-handler/chrome" = [ "firefox-devedition.desktop" ];
        "text/html" = [ "firefox-devedition.desktop" ];
        "application/x-extension-htm" = [ "firefox-devedition.desktop" ];
        "application/x-extension-html" = [ "firefox-devedition.desktop" ];
        "application/x-extension-shtml" = [ "firefox-devedition.desktop" ];
        "application/xhtml+xml" = [ "firefox-devedition.desktop" ];
        "application/x-extension-xhtml" = [ "firefox-devedition.desktop" ];
        "application/x-extension-xht" = [ "firefox-devedition.desktop" ];
      };
    };
  };
}
