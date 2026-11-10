# `features.<class>.<name>` is every feature module of a class, with a `key`. Profiles and hosts refer to
# features through it by attribute name (`with config.features.nixos; [ sway audio ]`), so a typo is an
# evaluation error at the line that wrote it. Modules with the same key are imported once, so a feature
# that is listed directly and also comes through a profile is harmless.
# The feature modules themselves are still defined as `flake.modules.<class>.<name>`.
{ lib, config, ... }: {
  options.features = {
    nixos = lib.mkOption {
      type = lib.types.attrsOf lib.types.deferredModule;
      readOnly = true;
      description = "NixOS feature modules by name, ready to be put in a host or a profile";
    };
    home = lib.mkOption {
      type = lib.types.attrsOf lib.types.deferredModule;
      readOnly = true;
      description = "home-manager feature modules by name, ready to be put in a host or a profile";
    };
  };

  config.features = {
    nixos =
      lib.mapAttrs
        (name: module: { key = "feature:nixos:${name}"; imports = [ module ]; })
        config.flake.modules.nixos;
    home =
      lib.mapAttrs
        (name: module: { key = "feature:home:${name}"; imports = [ module ]; })
        config.flake.modules.homeManager;
  };
}
