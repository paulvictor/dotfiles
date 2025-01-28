{ config, pkgs, lib, specialArgs, ...}:

{
  nixpkgs.overlays =
    [ (import ./packages/pass.nix)
      (self: super: {
        lib = super.lib // (import ./lib.nix super);
      })
      (import ./scripts/passdo.nix)
    ];
}
