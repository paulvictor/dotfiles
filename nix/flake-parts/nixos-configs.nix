{self, inputs, lib, perSystem, ...}:
let
  deviceConfigs =
    #attrset of host to configuration
    import ../root/devices/default.nix { inherit inputs lib; };
in
{
  flake.nixosConfigurations =
    lib.mapAttrs
      (_: modules:
        inputs.nixpkgs.lib.nixosSystem {
          inherit modules;
          specialArgs = {
            inherit inputs;
            isPhysicalDevice = true; # HACK for now
          };
        })
      deviceConfigs;
}
