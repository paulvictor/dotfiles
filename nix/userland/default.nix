args:

let
  inherit (args) pkgsFor homeManager impermanence flake-utils lib;
  mkHomeConfig = extraArgs: homeManager.lib.homeManagerConfiguration (rec {
    inherit (extraArgs) system;
    pkgs = pkgsFor system;
    configuration = {
      imports = [
        impermanence.nixosModules.home-manager.impermanence
        ./home-configuration.nix
      ];
    };
    username = "viktor";
    homeDirectory = "/home/viktor";
  } // extraArgs);
  allDevices = import ./all-devices.nix flake-utils.lib.system;
in
builtins.mapAttrs(_: attrs:
  let
    system = attrs.system;
  in
  mkHomeConfig {
    inherit system;
    pkgs = pkgsFor system;
    inherit (attrs) extraSpecialArgs;
  }) allDevices
