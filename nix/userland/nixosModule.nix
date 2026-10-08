{pkgs, config, lib, specialArgs, ...}:

{
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;
  home-manager.extraSpecialArgs = {inherit (specialArgs) inputs;};
  home-manager.backupFileExtension = ".bkp";
  home-manager.users.${config.me.username} = {
    imports = [
      specialArgs.inputs.nix-index-database.homeModules.nix-index
      ../modules/me.nix
      ./home-configuration.nix
      {
        home.username = config.me.username;
        home.homeDirectory = "/home/${config.me.username}";
        home.stateVersion = "24.05";
      }
    ]
    ++ (lib.optional (specialArgs.withGUI or true) ./gui-config.nix)
    ++ (lib.optional (specialArgs.isDevEnv or true)  ./dev-config.nix )
    ++ (lib.optional (specialArgs.isDesktop or true) ./desktop-config.nix);
  };
}
