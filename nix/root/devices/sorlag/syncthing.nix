{config, lib, pkgs, ...}:
with lib;
let
  syncthing-secret-generated = pathExists ./secrets/syncthing.yaml;
in
mkIf syncthing-secret-generated {
  sops.secrets."syncthing/key.pem" = {
    sopsFile = ./secrets/syncthing.yaml;
    format = "yaml";
    owner = config.users.users.${config.me.username}.name;
    group = config.users.users.${config.me.username}.group;
    restartUnits = [ "syncthing.service" ];
  };
  sops.secrets."syncthing/cert.pem" = {
    sopsFile = ./secrets/syncthing.yaml;
    format = "yaml";
    owner = config.users.users.${config.me.username}.name;
    group = config.users.users.${config.me.username}.group;
    restartUnits = [ "syncthing.service" ];
  };
}
