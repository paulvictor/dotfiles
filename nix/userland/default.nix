args:

let
  inherit (args) pkgsFor homeManager impermanence flake-utils lib nixpkgs nix-index-database;
  mkHomeConfig = extraArgs: homeManager.lib.homeManagerConfiguration (rec {
    pkgs = pkgsFor extraArgs.system;
    extraSpecialArgs = extraArgs.extraSpecialArgs;
    modules = [
      impermanence.nixosModules.home-manager.impermanence
      nix-index-database.hmModules.nix-index
      ./home-configuration.nix
    ] ++ [
      {
        home = {
          inherit (extraArgs) username homeDirectory stateVersion;
        };
      }
    ];
  });
  allDevices = import ./all-devices.nix flake-utils.lib.system nixpkgs;
in
builtins.mapAttrs(_: attrs:
  mkHomeConfig {
    inherit (attrs) system username homeDirectory stateVersion;
    pkgs = pkgsFor attrs.system;
    inherit (attrs) extraSpecialArgs;
  }) allDevices
