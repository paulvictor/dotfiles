args@{inputs, lib, overlays, ...}:

let
  system = inputs.flake-utils.lib.system.aarch64-darwin;
  devices = builtins.attrNames ( builtins.readDir ./devices);
  mkDarwinSystem = device: inputs.darwin.lib.darwinSystem {
    specialArgs = {inherit inputs;};
    modules = [
      {nixpkgs = {inherit overlays;};}
      inputs.homeManager.darwinModules.home-manager
      "${toString ./.}/devices/${device}/default.nix"
      ../userland/nix-darwin.nix
      ./modules/common.nix
      ./modules/tailscale.nix
    ];
  };


in
builtins.listToAttrs
  (lib.forEach devices (device: lib.nameValuePair device (mkDarwinSystem device)))
