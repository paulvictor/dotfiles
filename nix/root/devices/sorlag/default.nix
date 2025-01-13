# TODO : Look at https://github.com/openlab-aux/vuizvui for a better arrangement
{ config, pkgs, lib, ... }:

{
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "22.05"; # Did you read the comment?

  # From head -c4 /dev/urandom | od -A none -t x4
  networking.hostId = "1f03057e";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.efiSysMountPoint = "/boot";

  boot.kernelPackages = pkgs.linuxPackages_latest;

  services.logind.extraConfig = ''
    IdleAction=lock
    IdleActionSec=300
    HoldoffTimeoutSec=5
  '';

  imports = [
    ./hardware-configuration.nix
    ./syncthing.nix
  ];
  hardware.keyboard.qmk.enable = true;

  environment.systemPackages = with pkgs; [ qmk qmk_hid vial ];

#   hardware.video.hidpi.enable = true;

}
