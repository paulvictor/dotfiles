{ config, lib, pkgs, ... } :
with lib;
let
  encryptedSyncthingSecretPath = ../../devices/${config.networking.hostName}/secrets/syncthing.yaml;
in
mkIf (pathExists encryptedSyncthingSecretPath) {
  services.syncthing.enable = true;
  sops.secrets."syncthing/key.pem" = {
    sopsFile = encryptedSyncthingSecretPath;
    format = "yaml";
    owner = config.users.users.viktor.name;
    group = config.users.users.viktor.group;
    restartUnits = [ "syncthing.service" ];
  };
  sops.secrets."syncthing/cert.pem" = {
    sopsFile = encryptedSyncthingSecretPath;
    format = "yaml";
    owner = config.users.users.viktor.name;
    group = config.users.users.viktor.group;
    restartUnits = [ "syncthing.service" ];
  };
}
