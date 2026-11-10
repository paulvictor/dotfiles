# Nix itself. `nix` is all a lean host (cloud instance) needs; `nix-dev` and `emulation` are for
# machines that build things.
{ inputs, self, config, ... }: {
  flake.modules.nixos.nix = { lib, ... }: {
    imports = [ inputs.flake-utils-plus.nixosModules.autoGenFromInputs ];

    nixpkgs.overlays = lib.attrValues self.overlays;
    nixpkgs.config.allowUnfreePredicate = pkg:
      builtins.elem (lib.getName pkg) config.allowedUnfree;

    system.configurationRevision = lib.mkIf (self ? rev) self.rev;
    nix.generateNixPathFromInputs = true;
    nix.generateRegistryFromInputs = true;
    nix.linkInputs = true;

    nix.settings.auto-optimise-store = true;
    nix.extraOptions = ''
      experimental-features = nix-command flakes
      accept-flake-config = true
    '';

    boot.loader.grub.configurationLimit = lib.mkDefault 10;
    boot.loader.systemd-boot.configurationLimit = lib.mkDefault 10;
  };

  # Keeps build outputs around for development, newest nix, extra caches
  flake.modules.nixos.nix-dev = { pkgs, ... }: {
    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    nix.package = pkgs.nixVersions.latest;
    nix.settings.system-features = [ "kvm" "big-parallel" ];
    nix.settings.substituters = [ "https://nix-community.cachix.org" ];
    nix.settings.trusted-substituters = [ "https://nix-community.cachix.org" ];
    nix.settings.trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  # Run binaries of the other architectures through qemu-user (large closure)
  flake.modules.nixos.emulation = { lib, pkgs, ... }: {
    boot.binfmt.emulatedSystems =
      lib.remove
        pkgs.stdenv.hostPlatform.system
        [ "x86_64-linux" "aarch64-linux" "armv7l-linux" "riscv64-linux" ];
  };
}
