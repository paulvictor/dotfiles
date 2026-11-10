# The host registry and the only place that builds NixOS / home-manager configurations.
#
# A host picks profiles (flat named groups of features, see modules/profiles) and adds features of its own
# per class (`nixos.features`, `home.features`). Profiles are expanded and de-duplicated, so a host only
# ever ends up with a set of feature names. Each name must exist as `flake.modules.<class>.<name>`,
# otherwise evaluation fails.
{ inputs, lib, config, self, ... }:
let
  inherit (lib) mkOption types;
  top = config; # `config` is shadowed inside the host submodule
  flakeModules = config.flake.modules;

  names = description: mkOption {
    type = types.listOf types.str;
    default = [ ];
    apply = lib.unique;
    inherit description;
  };

  profile = name: top.profiles.${name} or (throw "Unknown profile '${name}'");

  # Feature names of the given profiles for one class
  profileFeatures = class: profileNames:
    lib.concatMap (name: (profile name).${class}) profileNames;

  profileType = types.submodule {
    options = {
      nixos = names "NixOS features, each needs a flake.modules.nixos.<name>";
      home = names "home-manager features, each needs a flake.modules.homeManager.<name>";
    };
  };

  hostType = types.submodule ({ config, ... }:
    let
      host = config;
      resolve = class: lib.unique (profileFeatures class host.profiles ++ host.${class}.features);
    in
    {
      options = {
        system = mkOption {
          type = types.str;
          default = "x86_64-linux";
        };
        profiles = names "Profiles to take features from";
        nixos = {
          enable = mkOption { type = types.bool; default = true; };
          stateVersion = mkOption { type = types.str; };
          features = names "NixOS features on top of the profiles";
          resolved = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            default = resolve "nixos";
            description = "Every NixOS feature of this host, without duplicates";
          };
          modules = mkOption {
            type = types.listOf types.deferredModule;
            default = [ ];
            description = "Host specific NixOS modules (hardware, overrides)";
          };
        };
        home = {
          enable = mkOption { type = types.bool; default = true; };
          stateVersion = mkOption { type = types.str; };
          features = names "home-manager features on top of the profiles";
          resolved = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            default = resolve "home";
            description = "Every home-manager feature of this host, without duplicates";
          };
          modules = mkOption {
            type = types.listOf types.deferredModule;
            default = [ ];
            description = "Host specific home-manager modules (outputs, battery, ...)";
          };
        };
      };
    });

  hostsFor = pred: lib.filterAttrs (_: pred) config.hosts;
in
{
  options.profiles = mkOption {
    type = types.attrsOf profileType;
    default = { };
    description = "Named groups of features";
  };

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
                system.stateVersion = host.nixos.stateVersion;
              }]
              ++ map (feature: flakeModules.nixos.${feature}) host.nixos.resolved
              ++ host.nixos.modules;
          })
        (hostsFor (host: host.nixos.enable));

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
                    [{ home.stateVersion = host.home.stateVersion; }]
                    ++ map (feature: flakeModules.homeManager.${feature}) host.home.resolved
                    ++ host.home.modules;
                }))
            (hostsFor (host: host.home.enable && host.system == system));
      };
  };
}
