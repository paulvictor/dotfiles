# TODO : Look at https://github.com/openlab-aux/vuizvui for a better arrangement
{ config, pkgs, ... }:

let
  syncthing-key-file = "/run/syncthing-key.pem";
  syncthing-cert-file = "/run/syncthing-cert.pem";
in
{
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.09"; # Did you read the comment?

  networking.hostName = "sarge";
  # From head -c4 /dev/urandom | od -A none -t x4
  networking.hostId = "d11572e4";
  imports = [ ./hardware-configuration.nix ];
  boot.loader.systemd-boot.enable = false;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.kernelPackages = pkgs.linuxPackages_5_15;

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
    syncthing-key = {
      key = "syncthing/key.pem";
      owner = config.users.users.viktor.name;
      mode = "0400";
      path = syncthing-key-file;
    };
    syncthing-cert = {
      key = "syncthing/cert.pem";
      owner = config.users.users.viktor.name;
      mode = "0400";
      path = syncthing-cert-file;
    };
  };

  services.syncthing = {
    enable = true;
    user = config.users.users.viktor.name;
    key = syncthing-key-file;
    cert = syncthing-cert-file;
    devices = {
      sarge = {
        id = "OWYAVCE-HZNWJAH-CX4XEOR-ECZKTEB-W6YXQTB-B52HOXA-BRT7PBB-J3OQIQ7";
        name = config.networking.hostName;
        introducer = true;
      };
      uriel = {
        id = "IYLKTPE-SF5YCW4-XXC6C5H-JJF6IZL-NMXDFL5-G2JDDBE-3QS4GSD-I4J3IAC";
        name = "uriel";
        introducer = true;
      };
    };
    folders = {
      "/persist/home/viktor/crypt" = {
        id = "persistent-home";
        devices = [ "uriel" "sarge" ];
      };
    };
  };
}
