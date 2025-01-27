args:

let
  inherit (args) pkgs inputs overlays;
  inherit (inputs) magix guile-swayer;
  mkHomeConfig = extraArgs: inputs.homeManager.lib.homeManagerConfiguration (rec {
    inherit pkgs;
    extraSpecialArgs = extraArgs.extraSpecialArgs // {inherit guile-swayer magix overlays; inherit (extraArgs) system;};
    modules = [
      inputs.nix-index-database.hmModules.nix-index
      ./overlays.nix
      ./home-configuration.nix
      {nixpkgs.config = {allowUnfree = true;};}
    ] ++ [
      {
        home = {
          inherit (extraArgs) username homeDirectory stateVersion;
        };
      }
    ];
  });
  allDevices = import ./all-devices.nix inputs.flake-utils.lib.system;
in
builtins.mapAttrs(_: attrs:
  mkHomeConfig {
    inherit (attrs) system username homeDirectory stateVersion extraSpecialArgs;
    inherit pkgs;
  }) allDevices
