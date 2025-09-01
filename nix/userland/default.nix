{pkgs, inputs, ...}:
let
  inherit (pkgs) lib;
in
builtins.mapAttrs(_: attrs:
  inputs.homeManager.lib.homeManagerConfiguration ({
    inherit pkgs;
    extraSpecialArgs = {inherit inputs;};
    modules = [
      inputs.nix-index-database.hmModules.nix-index
      ./home-configuration.nix
      {
        home.username = lib.mkDefault "viktor";
        home.homeDirectory = lib.mkDefault "/home/viktor";
      }
    ]
    ++ (lib.optional (attrs.withGUI or true) ./gui-config.nix)
    ++ (lib.optional (attrs.isDevEnv or true)  ./dev-config.nix )
    ++ (lib.optional (attrs.isDesktop or true) ./desktop-config.nix)
    ++ attrs.additionalModules;
  })) (import ./all-devices.nix)
