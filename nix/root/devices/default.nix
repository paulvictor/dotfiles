{ lib , system , homeManager , self, sops-nix, nixpkgs, pkgs, nixos-generators }:

let
  inherit (builtins) attrNames isAttrs readDir listToAttrs;

  inherit (lib) filterAttrs hasSuffix mapAttrs' nameValuePair removeSuffix;

  inherit (lib) hasPrefix;
  mkModules = hostName:
    let
      common = {
        imports = [
          ../guix/modules/services/guix.nix
          # TODO : Move this to distributed modules
          # Since hostName is available, use that to determine what components are needed
          ../common-config.nix
        ];
        hardware.enableRedistributableFirmware = lib.mkDefault true;

        networking.hostName = hostName;
        system.configurationRevision = lib.mkIf (self ? rev) self.rev;
        users.mutableUsers = false;

        nixpkgs.pkgs = pkgs;
        nix.registry.nixpkgs.flake = nixpkgs;
      };
      machine = import "${toString ./.}/${hostName}/default.nix";
    in [
      common
      sops-nix.nixosModule
      homeManager.nixosModule
      machine
    ];

  mkSystem = hostName:
    lib.nixosSystem {
      inherit system pkgs;
      modules = mkModules hostName;
    };

  mkImage = hostName: format:
    nixos-generators.nixosGenerate {
      inherit pkgs format;
      modules = mkModules hostName;
    };

  forAllFormats = lib.genAttrs [ "install-iso" "iso" ];

  forAllDevices =
    lib.genAttrs (attrNames (readDir ./.));

  systems = forAllDevices mkSystem;

  medias = forAllDevices (device:
    forAllFormats (format: mkImage device format));

in

systems // { inherit medias; }
