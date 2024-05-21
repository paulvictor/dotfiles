args@{ config, pkgs, lib, ...}:

let
  inherit (args) isPhysicalDevice;
  customisations = args.customisations or {};
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
  nix.package = pkgs.nixFlakes;
  nix.settings.system-features = [ "kvm" "big-parallel" ];

  hardware.enableRedistributableFirmware = mkDefault true;

  users.mutableUsers = false;

  services.guix = {
    enable = isPhysicalDevice;
    gc.enable = true;
  };

  boot.binfmt.emulatedSystems =
    optionals
      isPhysicalDevice
        [ "aarch64-linux" "armv7l-linux" "riscv64-linux" ];

  boot.postBootCommands = "
    [ -d /tomb/${config.networking.hostName}/ssh ] || \
      mkdir -pv /tomb/${config.networking.hostName}/ssh
  ";

  imports =
    optionals isPhysicalDevice [
      ./modules/desktop-environment.nix
      ./modules/networking.nix
    ] ++
    optionals (customisations.onZFS or false) [
      ./modules/impermanence-zfs.nix
    ];
}
