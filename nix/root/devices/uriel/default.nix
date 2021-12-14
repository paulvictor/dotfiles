# TODO : Look at https://github.com/openlab-aux/vuizvui for a better arrangement
{ config, pkgs, ... }:

{
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "21.05"; # Did you read the comment?

  networking.hostName = "uriel";
  # From head -c4 /dev/urandom | od -A none -t x4
  networking.hostId = "237ab499";
  imports = [ ./hardware-configuration.nix ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.kernelPackages = pkgs.linuxPackages_5_14;

  services.logind.extraConfig = ''
    IdleAction=lock
    IdleActionSec=300
    HoldoffTimeoutSec=5
  '';

  sops.defaultSopsFile = ./secrets.yaml;

  sops.gnupg.sshKeyPaths = [ "/tomb/${config.networking.hostName}/ssh/ssh_host_rsa_key" ];
  sops.secrets = {
    crypt-mount-key = {
      key = "home-persistence/key";
      owner = config.users.users.viktor.name;
      mode = "0400";
    };
  };
}
