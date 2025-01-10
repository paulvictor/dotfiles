{ config, pkgs, lib, specialArgs, ...}:

{
  nixpkgs.overlays =
    specialArgs.overlays ++
    [ (import ./packages/pass.nix)
      (self: super: {
        lib = super.lib // (import ./lib.nix super);
      })
      (import ./scripts/passdo.nix)
    ];
}
