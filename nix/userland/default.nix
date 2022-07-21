args:

let
  inherit (args) pkgsFor homeManager impermanence flake-utils lib nixpkgs;
  mkHomeConfig = extraArgs: homeManager.lib.homeManagerConfiguration (rec {
    inherit (extraArgs) system;
    pkgs = pkgsFor system;
    configuration = {
      imports = [
        impermanence.nixosModules.home-manager.impermanence
        ./home-configuration.nix
      ];
    };
  } // extraArgs);
  allDevices = import ./all-devices.nix flake-utils.lib.system nixpkgs;
in
builtins.mapAttrs(_: attrs:
  mkHomeConfig {
    inherit (attrs) system username homeDirectory;
    pkgs = pkgsFor attrs.system;
    inherit (attrs) extraSpecialArgs;
  }) allDevices
