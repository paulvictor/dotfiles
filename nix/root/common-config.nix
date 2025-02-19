{ config, pkgs, lib, isPhysicalDevice, ...}:

with lib;
{
  nix.settings.auto-optimise-store = true;
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    experimental-features = nix-command flakes
    accept-flake-config = true
  '';
  nix.package = pkgs.nixVersions.latest;
  nix.settings.system-features = [ "kvm" "big-parallel" ];

  hardware.enableRedistributableFirmware = mkDefault true;

  users.mutableUsers = false;

  services.guix = {
    enable = false;
    gc.enable = true;
  };

  boot.binfmt.emulatedSystems =
    lib.remove
      pkgs.stdenv.hostPlatform.system
      [ "x86_64-linux" "aarch64-linux" "armv7l-linux" "riscv64-linux" ];

  imports =
    optionals isPhysicalDevice [
      ./modules/desktop-environment.nix
      ./modules/networking.nix
      ./deploy-secrets.nix
    ];
}
