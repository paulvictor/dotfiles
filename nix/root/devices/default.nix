{ inputs, overlays, lib, ... }:

let

  inherit (builtins) attrNames isAttrs readDir listToAttrs elem;

  inherit (lib) forEach;

  commonModules =
    let
      common = {lib, ...}: {
        nixpkgs.overlays = overlays;
        system.configurationRevision = lib.mkIf (inputs.self ? rev) inputs.self.rev;
      };
    in [

      ../common-config.nix
      ../caches.nix
      common
      {
        nix.generateNixPathFromInputs = true;
        nix.generateRegistryFromInputs = true;
        nix.linkInputs = true;
      }
      ../modules/viktor.nix
      ../modules/workstations.nix
      ../modules/ssh.nix
    ];
  deviceConfigs = import ./all-devices.nix;
in
listToAttrs
  (forEach
    deviceConfigs
    (deviceConfig:
      let
        inherit (deviceConfig) hostName unifiedHomeManager;
      in
        {
          name = hostName;
          value = [
            inputs.flake-utils-plus.nixosModules.autoGenFromInputs
            inputs.sops-nix.nixosModules.sops
            inputs.homeManager.nixosModule
            {
              networking.hostName = hostName;
            }
            "${toString ./.}/${hostName}/default.nix"
          ] ++ commonModules ++ (lib.optionals unifiedHomeManager [../../userland/nixosModule.nix]);
        }))
