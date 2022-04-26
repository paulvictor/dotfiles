# TODO : Look at https://github.com/openlab-aux/vuizvui for a better arrangement
# { config, pkgs, ... }:

{ config, pkgs, lib, ... }:

{
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.09"; # Did you read the comment?

  networking.hostName = "sarge";
  # From head -c4 /dev/urandom | od -A none -t x4
  networking.hostId = "d11572e4";
  boot.loader.systemd-boot.enable = false;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.kernelPackages = pkgs.linuxPackages_5_15;

  services.logind.extraConfig = ''
    IdleAction=lock
    IdleActionSec=300
    HoldoffTimeoutSec=5
  '';

  imports = [
    ./hardware-configuration.nix
    ./syncthing.nix
    ../../modules/desktop-environment.nix
    ../../modules/impermanence-zfs.nix
    ../../modules/networking.nix
    ../../modules/viktor.nix
    ../../modules/workstations.nix
  ];

}
