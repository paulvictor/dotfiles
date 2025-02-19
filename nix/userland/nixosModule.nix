{pkgs, config, lib, specialArgs, ...}:

{
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;
  home-manager.extraSpecialArgs = {inherit (specialArgs) inputs;};
  home-manager.backupFileExtension = ".bkp";
  home-manager.users.viktor = {
    imports = [
      specialArgs.inputs.nix-index-database.hmModules.nix-index
      ./home-configuration.nix
      {
        home.username = "viktor";
        home.homeDirectory = "/home/viktor";
        home.stateVersion = "24.05";
      }
    ]
    ++ (lib.optional (specialArgs.withGUI or true) ./gui-config.nix)
    ++ (lib.optional (specialArgs.isDevEnv or true)  ./dev-config.nix )
    ++ (lib.optional (specialArgs.isDesktop or true) ./desktop-config.nix);
  };
}
# builtins.mapAttrs(_: attrs:
#   let
#     inherit (attrs) extraSpecialArgs additionalModules;
#   in ({

#     extraSpecialArgs = extraSpecialArgs // {inherit magix;};
#     modules = [
#       inputs.nix-index-database.hmModules.nix-index
#       ./overlays.nix
#       ./home-configuration.nix
#       {
#         nixpkgs.config.allowUnfree = true;
#         nixpkgs.overlays = overlays;
#       }
#     ]
#     ++ (lib.optional attrs.withGUI ./gui-config.nix)
#     ++ (lib.optional attrs.isDevEnv  ./dev-config.nix )
#     ++ (lib.optional attrs.isDesktop ./desktop-config.nix)
#     ++ additionalModules;
#   })) allDevices

