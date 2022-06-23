{ lib , homeManager , self, sops-nix, nixpkgs, pkgsFor, nixos-generators, flake-utils, inputs, ... }:

let
  inherit (builtins) attrNames isAttrs readDir listToAttrs elem;

  inherit (lib) filterAttrs hasSuffix mapAttrs' nameValuePair removeSuffix;

  inherit (lib) hasPrefix forEach;

  setupNixPath = {config, lib, ...}: {
    environment.etc =
      mapAttrs'
        (name: value: { name = "nix/inputs/${name}"; value = { source = value.outPath; }; })
        inputs;
    nix.nixPath = [ "/etc/nix/inputs" ];
  };

  mkModules = hostName: system:
    let
      common = {
        imports = [
          ../guix/modules/services/guix.nix
          ../common-config.nix
        ];

        system.configurationRevision = lib.mkIf (self ? rev) self.rev;
        networking.hostName = hostName;
        nixpkgs.pkgs = pkgsFor system;
        nix.registry.nixpkgs.flake = nixpkgs;
      };

      machine = import "${toString ./.}/${hostName}/default.nix";

    in [
      setupNixPath
      common
      sops-nix.nixosModule
      homeManager.nixosModule
      machine
      ../modules/viktor.nix
      ../modules/workstations.nix
      ../modules/ssh.nix
    ];

  mkNixosSystem = hostName: system:
    let
      pkgs = pkgsFor system;
    in lib.nixosSystem {
      inherit system pkgs;
      modules = mkModules hostName system;
      specialArgs = {
        isPhysicalDevice = true;
      };
    };

  deviceConfigs = import ./all-devices.nix;

in
listToAttrs
  (forEach
    deviceConfigs
    (deviceConfig:
      let
        inherit (deviceConfig) hostName system extraModules format isPhysicalDevice;
        generatedImage = nixos-generators.nixosGenerate {
          pkgs = pkgsFor system;
          format = format;
          modules =
            mkModules hostName system
            ++ (lib.optionals (deviceConfig ? extraModules) deviceConfig.extraModules);
          specialArgs = {
            isPhysicalDevice = elem format [ "iso" "install-iso" ];
          };
        };
      in
        {
          name = hostName;
          value =
            if !(deviceConfig ? format)
            then mkNixosSystem hostName system
            else generatedImage;
        }))
