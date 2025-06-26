{ inputs, overlays, lib, ... }:

let

  inherit (builtins) attrNames isAttrs readDir listToAttrs elem;

  inherit (lib) forEach;

  commonModules =
    [
      ../common-config.nix
      ../caches.nix
      ({lib, ...}: {
        system.configurationRevision = lib.mkIf (inputs.self ? rev) inputs.self.rev;
        nixpkgs.overlays = overlays;
        nix.generateNixPathFromInputs = true;
        nix.generateRegistryFromInputs = true;
        nix.linkInputs = true;
        nixpkgs.config.allowUnfreePredicate =
          pkg: builtins.elem (lib.getName pkg)
            [
              "google-chrome"
              "vivaldi"
              "widevine-cdm"
              "ungoogled-chromium"
              "ungoogled-chromium-unwrapped"
              "prl-tools"
              "firefox-devedition-bin"
              "firefox-developer-edition-bin-unwrapped"
            ];
      })
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
            inputs.homeManager.nixosModules.default
            { networking.hostName = hostName; }
            "${toString ./.}/${hostName}/default.nix"
          ] ++ commonModules ++ (lib.optionals unifiedHomeManager [../../userland/nixosModule.nix]);
        }))
