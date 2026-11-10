# The type of a profile: flat lists of feature modules per class (config.features.<class>.<name>).
# Used by `profiles` and by the `profiles` list of a host.
{ lib }:
lib.types.submodule {
  options = {
    nixos = lib.mkOption {
      type = lib.types.listOf lib.types.deferredModule;
      default = [ ];
      description = "NixOS features, from config.features.nixos";
    };
    home = lib.mkOption {
      type = lib.types.listOf lib.types.deferredModule;
      default = [ ];
      description = "home-manager features, from config.features.home";
    };
  };
}
