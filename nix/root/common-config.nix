args@{ config, pkgs, lib, ...}:

let
  inherit (args) isPhysicalDevice;
in
with lib;
{
  nix.settings.auto-optimise-store = true;
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    experimental-features = nix-command flakes
    accept-flake-config = true
  '';
  nix.package = pkgs.nixVersions.stable;
  nix.settings.system-features = [ "kvm" "big-parallel" ];

  hardware.enableRedistributableFirmware = mkDefault true;

  users.mutableUsers = false;

  services.guix = {
    enable = false;
    gc.enable = true;
  };

  boot.binfmt.emulatedSystems =
    optionals
      isPhysicalDevice
        [ "aarch64-linux" "armv7l-linux" "riscv64-linux" ];

  imports =
    optionals isPhysicalDevice [
      ./modules/desktop-environment.nix
      ./modules/networking.nix
      ./deploy-secrets.nix
    ];
}
