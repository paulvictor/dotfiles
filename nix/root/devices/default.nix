{ lib , system , homeManager , self, sops-nix, nixpkgs, pkgs }:

let
  inherit (builtins) attrNames isAttrs readDir listToAttrs;

  inherit (lib) filterAttrs hasSuffix mapAttrs' nameValuePair removeSuffix;

  inherit (lib) hasPrefix;

  mkSystem = hostName:
    lib.nixosSystem {
      inherit system;
      inherit pkgs;

      modules = let
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
      in
        [
          common
          sops-nix.nixosModule
          homeManager.nixosModule
          machine
        ];
    };
  recImportDirs = dir:
    listToAttrs
      (map
        (n: { name = n; value = mkSystem n; })
        (attrNames (readDir dir)));

in
recImportDirs (toString ./.)
