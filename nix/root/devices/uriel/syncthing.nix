{ config, lib, pkgs, ... } :

let
  syncthing-key-file = "/run/syncthing-key.pem";
  syncthing-cert-file = "/run/syncthing-cert.pem";
in
{
  imports = [
    ../../modules/syncthing.nix
  ];

  sops.defaultSopsFile = ./secrets.yaml;

  sops.gnupg.sshKeyPaths = [ "/tomb/${config.networking.hostName}/ssh/ssh_host_rsa_key" ];
  sops.secrets = {
#    crypt-mount-key = {
#      key = "home-persistence/key";
#      owner = config.users.users.viktor.name;
#      mode = "0400";
#    };
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
    user = "viktor";
    key = syncthing-key-file;
    cert = syncthing-cert-file;
    folders = {
      "/persist/home/viktor/crypt" = {
        id = "persistent-home";
        devices = [ "uriel" "sarge" ];
      };
    };
  };
}
