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
  '';
  nix.package = pkgs.nixFlakes;
  nix.settings.system-features = [ "kvm" "big-parallel" ];

  hardware.enableRedistributableFirmware = mkDefault true;

  users.mutableUsers = false;

  boot.binfmt.emulatedSystems =
    optionals
      isPhysicalDevice
        [ "aarch64-linux" "armv7l-linux" "riscv64-linux" ];

  services.guix.enable = isPhysicalDevice;

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
