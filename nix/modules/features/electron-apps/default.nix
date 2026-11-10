# Web apps wrapped as electron apps. Any module can add one:
#   programs.electronApps.<name> = { url = "https://..."; proxy = "socks5://host:port"; };
# and a rofi runner that lists them is installed alongside.
{
  flake.overlays.electron-apps = final: prev: {
    mkElectronApp = final.callPackage ./_apps/mk-app.nix { };
  };

  flake.modules.homeManager.electron-apps = { config, lib, pkgs, ... }:
    let
      cfg = config.programs.electronApps;
      electronApps = pkgs.symlinkJoin {
        name = "electron-apps";
        paths = lib.mapAttrsToList (name: app: pkgs.mkElectronApp ({ inherit name; } // app)) cfg;
      };
    in
    {
      options.programs.electronApps = lib.mkOption {
        default = { };
        description = "Web apps to wrap as electron apps, by name";
        type = lib.types.attrsOf (lib.types.submodule {
          options = {
            url = lib.mkOption {
              type = lib.types.str;
              description = "Page to open";
            };
            proxy = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              example = "socks5://localhost:1080";
              description = "Proxy server for this app only";
            };
          };
        });
      };

      config = {
        programs.electronApps = {
          Gitter.url = "https://gitter.im";
          BrainFm.url = "https://my.brain.fm/";
          YoutubeMusic.url = "https://music.youtube.com";
        };

        home.packages = lib.mkIf (cfg != { }) [
          electronApps
          (pkgs.callPackage ./_apps/rofiRun.nix { inherit electronApps; })
        ];
      };
    };
}
