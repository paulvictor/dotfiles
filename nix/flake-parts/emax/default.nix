{
  # Linux only: EAF, GIO modules etc. in package.nix
  perSystem = { pkgs, lib, ... }: {
    packages = lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
      emacs = pkgs.callPackage ./standalone.nix {
        emacs = pkgs.callPackage ./package.nix { };
      };
    };
  };
}
