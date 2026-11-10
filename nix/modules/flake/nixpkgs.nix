# One nixpkgs for the flake (perSystem `pkgs`, used by home-manager and packages).
# Features append to `allowedUnfree`; NixOS hosts use the same list (see features/base).
{ inputs, lib, self, config, ... }: {
  options.allowedUnfree = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = "Names of unfree packages allowed in NixOS and home-manager";
  };

  config.perSystem = { system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = lib.attrValues self.overlays;
      config.allowUnfreePredicate = pkg:
        builtins.elem (lib.getName pkg) config.allowedUnfree;
    };
  };
}
