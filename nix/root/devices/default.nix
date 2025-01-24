{ inputs, overlays, lib, ... }:

let

  inherit (builtins) attrNames isAttrs readDir listToAttrs elem;

  inherit (lib) nixosSystem forEach;

  setupNixPath = {lib, ...}: {
    environment.etc =
      lib.mapAttrs'
        (name: value: { name = "nix/inputs/${name}"; value = { source = value.outPath; }; })
        inputs;
    nix.nixPath = [ "/etc/nix/inputs" ];
  };

  mkModules = hostName:
    let
      common = {lib, ...}: {
        imports = [
          ../common-config.nix
          ../caches.nix
#           ../tailscale.nix
          ../modules/actual-server.nix
        ];
        nixpkgs.overlays = overlays;
        system.configurationRevision = lib.mkIf (inputs.self ? rev) inputs.self.rev;
        networking.hostName = hostName;
      };
      machine = import "${toString ./.}/${hostName}/default.nix";
    in [
      setupNixPath
      common
      inputs.sops-nix.nixosModules.sops
      inputs.homeManager.nixosModule
      machine
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
        inherit (deviceConfig) hostName;
      in
        {
          name = hostName;
          value = mkModules hostName;
        }))
