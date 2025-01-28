{pkgs, inputs, overlays, ...}:

let
  inherit (inputs) magix homeManager;
  allDevices = import ./all-devices.nix;
  inherit (pkgs) lib;
in
builtins.mapAttrs(_: attrs:
  let
    inherit (attrs) extraSpecialArgs additionalModules;
  in homeManager.lib.homeManagerConfiguration ({
    inherit pkgs;
    extraSpecialArgs = extraSpecialArgs // {inherit magix;};
    modules = [
      inputs.nix-index-database.hmModules.nix-index
      ./overlays.nix
      ./home-configuration.nix
      {
        nixpkgs.config.allowUnfree = true;
        nixpkgs.overlays = overlays;
      }
    ]
    ++ (lib.optional attrs.withGUI ./gui-config.nix)
    ++ (lib.optional attrs.isDevEnv  ./dev-config.nix )
    ++ (lib.optional attrs.isDesktop ./desktop-config.nix)
    ++ additionalModules;
  })) allDevices
