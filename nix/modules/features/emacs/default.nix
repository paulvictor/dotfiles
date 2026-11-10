# Emacs: the daemon on home-manager, and a standalone `nix run .#emacs` with the config baked in
{
  flake.overlays.wuzapi = import ./_wuzapi.nix;

  flake.modules.homeManager.emacs = { pkgs, lib, ... }:
    let
      package = pkgs.callPackage ./_package.nix { };
    in
    {
      home.packages = let fonts = import ./_fonts.nix pkgs; in fonts.used ++ fonts.trial;

      services.emacs = {
        enable = pkgs.stdenv.hostPlatform.isLinux;
        inherit package;
        client = {
          enable = true;
          arguments = [ "-c" "-n" "-a" "emacs" ];
        };
      };

      programs.emacs = {
        enable = true;
        inherit package;
      };

      systemd.user.sessionVariables = lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
        EDITOR = "emacsclient -c -n";
      };
    };

  # Linux only: EAF, GIO modules etc. in _package.nix
  perSystem = { pkgs, lib, ... }: {
    packages = lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
      emacs = pkgs.callPackage ./_standalone.nix {
        emacs = pkgs.callPackage ./_package.nix { };
        fonts = (import ./_fonts.nix pkgs).used;
      };
    };
  };
}
