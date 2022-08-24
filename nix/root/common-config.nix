args@{ config, pkgs, lib, ...}:

let
  inherit (args) isPhysicalDevice;
in
{
  nix.settings.auto-optimise-store = true;
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    experimental-features = nix-command flakes
  '';
  nix.package = pkgs.nixFlakes;
  nix.settings.system-features = [ "kvm" "big-parallel" ];

  hardware.enableRedistributableFirmware = lib.mkDefault true;

  users.mutableUsers = false;

  boot.binfmt.emulatedSystems =
    lib.optionals
      isPhysicalDevice
        [ "aarch64-linux" "armv7l-linux" "riscv64-linux" ];

  services.guix.enable = isPhysicalDevice;

  boot.kernelPackages = pkgs.linuxPackages_5_18;

  boot.postBootCommands = "
    [ -d /tomb/${config.networking.hostName}/ssh ] || \
      mkdir -pv /tomb/${config.networking.hostName}/ssh
  ";

  imports =
    lib.optionals isPhysicalDevice [
      ./modules/desktop-environment.nix
      ./modules/impermanence-zfs.nix
      ./modules/networking.nix
    ];
}
