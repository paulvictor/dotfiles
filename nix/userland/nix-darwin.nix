{pkgs, config, lib, specialArgs, ...}:

{
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;
  home-manager.extraSpecialArgs = {inherit (specialArgs) inputs;};
  home-manager.backupFileExtension = ".bkp";
  home-manager.users."paul.victor" = {
    imports = [
      specialArgs.inputs.nix-index-database.hmModules.nix-index
      ./home-configuration.nix
      {
        home.username = "paul.victor";
        home.homeDirectory = "/Users/paul.victor";
        home.stateVersion = "25.05";
      }
    ]
   ++ (lib.optional (specialArgs.withGUI or true) ./gui-config.nix)
   ++ (lib.optional (specialArgs.isDevEnv or true)  ./dev-config.nix );
#    ++ (lib.optional (specialArgs.isDesktop or true) ./desktop-config.nix);
  };
}
