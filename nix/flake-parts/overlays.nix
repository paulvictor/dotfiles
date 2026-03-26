{inputs, lib, ...}:

let
  #   dyalog-nixos-overlay = import (fetchTarball https://github.com/markus1189/dyalog-nixos/tarball/3e09260ec111541be3e0c7a6c4e700fc042a3a8a) { inherit pkgs; } ;
  brotab-overlay = import ../overlays/brotab.nix;
  rofi-fuzzy = import ../overlays/rofi-fuzzy.nix;
  pass-override-overlay = import ../overlays/pass-override.nix;
  pass-with-extensions = import ../overlays/pass-with-extensions.nix;
  wallpaper-overlay = import ../overlays/wallpaper.nix;
  electron-apps = import ../overlays/electronApps;
  rofi-theme-overlay = import ../overlays/rofi-theme-overlay.nix;
  warpd-overlay = import ../overlays/warpd.nix;
  passdo = import ../overlays/type-password/passdo.nix;
  schemesh-overlay = import ../overlays/schemesh.nix;

  composed =
    lib.composeManyExtensions
      [
        (final: _: (inputs.vieb-nix.packagesFunc final))
        (schemesh-overlay inputs.schemesh)
        brotab-overlay
        rofi-fuzzy
        pass-override-overlay
        pass-with-extensions
        passdo
        electron-apps
        wallpaper-overlay
        inputs.nur.overlays.default
        inputs.emacsOverlay.overlay
        rofi-theme-overlay
        warpd-overlay
      ];

in {
  flake.overlays.default = composed;
}
