# The host registry and the only place that builds NixOS / home-manager configurations.
#
# A host picks profiles (see profiles.nix) and adds features of its own per class (`nixos.features`,
# `home.features`). Both refer to features by attribute name, see features.nix; duplicates collapse because
# the feature modules carry a key.
{ inputs, lib, config, self, ... }:
let
  inherit (lib) mkOption types;

  hostType = types.submodule {
    options = {
      system = mkOption {
        type = types.str;
        default = "x86_64-linux";
      };
      stateVersion = mkOption {
        type = types.str;
        description = "system.stateVersion and home.stateVersion; the release the machine was first installed with";
      };
      profiles = mkOption {
        type = types.listOf (import ../_profile-type.nix { inherit lib; });
        default = [ ];
        description = "Profiles to take features from, from config.profiles";
      };
      nixos = {
        enable = mkOption { type = types.bool; default = true; };
        features = mkOption {
          type = types.listOf types.deferredModule;
          default = [ ];
          description = "NixOS features on top of the profiles, from config.features.nixos";
        };
        modules = mkOption {
          type = types.listOf types.deferredModule;
          default = [ ];
          description = "Host specific NixOS modules (hardware, overrides)";
        };
      };
      home = {
        enable = mkOption { type = types.bool; default = true; };
        features = mkOption {
          type = types.listOf types.deferredModule;
          default = [ ];
          description = "home-manager features on top of the profiles, from config.features.home";
        };
        modules = mkOption {
          type = types.listOf types.deferredModule;
          default = [ ];
          description = "Host specific home-manager modules (outputs, battery, ...)";
        };
      };
    };
  };
in
{
  options.hosts = mkOption {
    type = types.attrsOf hostType;
    default = { };
  };

  config = {
    flake.nixosConfigurations =
      lib.mapAttrs
        (name: host:
          inputs.nixpkgs.lib.nixosSystem {
            specialArgs = { inherit inputs; };
            modules =
              [{
                networking.hostName = name;
                nixpkgs.hostPlatform = lib.mkDefault host.system;
                system.stateVersion = host.stateVersion;
              }]
              ++ lib.concatMap (profile: profile.nixos) host.profiles
              ++ host.nixos.features
              ++ host.nixos.modules;
          })
        (lib.filterAttrs (_: host: host.nixos.enable) config.hosts);

    # Under legacyPackages because of
    # https://github.com/nix-community/home-manager/issues/3075#issuecomment-3037360368
    perSystem = { pkgs, system, ... }:
      let
        # Evaluated standalone so the primary username can name the configs
        # (home-manager looks up `$USER@$HOSTNAME` by default)
        me = (lib.evalModules {
          modules = [ ../_me-options.nix { _module.args = { inherit pkgs; }; } ];
        }).config.me;
      in
      {
        legacyPackages.homeConfigurations =
          lib.mapAttrs'
            (name: host:
              lib.nameValuePair "${me.username}@${name}"
                (inputs.homeManager.lib.homeManagerConfiguration {
                  inherit pkgs;
                  extraSpecialArgs = { inherit inputs; };
                  modules =
                    [{ home.stateVersion = host.stateVersion; }]
                    ++ lib.concatMap (profile: profile.home) host.profiles
                    ++ host.home.features
                    ++ host.home.modules;
                }))
            (lib.filterAttrs (_: host: host.home.enable && host.system == system) config.hosts);
      };
  };
}
