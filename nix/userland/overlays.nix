{ config, pkgs, lib, specialArgs, ...}:

{
  nixpkgs.overlays =
    [
      (self: super: {
        lib = super.lib // (import ./lib.nix super);
      })
      (import ./scripts/passdo.nix)
    ];
}
