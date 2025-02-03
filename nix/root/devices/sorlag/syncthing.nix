{ config, ... } :

{
  imports = [
    ../../modules/syncthing.nix
  ];
  sops.secrets."syncthing/key.pem" = {
    sopsFile = ./secrets/syncthing.yaml;
    format = "yaml";
    owner = config.users.users.viktor.name;
    group = config.users.users.viktor.group;
    restartUnits = [ "syncthing.service" ];
  };
  sops.secrets."syncthing/cert.pem" = {
    sopsFile = ./secrets/syncthing.yaml;
    format = "yaml";
    owner = config.users.users.viktor.name;
    group = config.users.users.viktor.group;
    restartUnits = [ "syncthing.service" ];
  };

  services.syncthing = {
    enable = true;
    user = "viktor";
    openDefaultPorts = true;
    configDir = "${config.users.users.viktor.home}/.config/syncthing";
    key = config.sops.secrets."syncthing/key.pem".path;
    cert = config.sops.secrets."syncthing/cert.pem".path;
    settings = {
      options = {
        urAccepted = -1;
        relaysEnabled = true;
        localAnnounceEnabled = true;
      };
      folders = {
        "${config.users.users.viktor.home}/Photos/victor-pixel8-photos" = {
          id = "bmce4-0zr5d";
          label = "Photos On Victors Pixel 8";
          devices = [ "uriel" "victors-phone" "sorlag" ];
          type = "receiveonly";
          copyOwnershipFromParent = true;
        };
        "${config.users.users.viktor.home}/org-roam-notes" = {
          id = "roam-notes";
          label = "Org roam notes";
          copyOwnershipFromParent = true;
          versioning = {
            type = "simple";
            params.keep = "10";
          };
          devices = [
            "uriel"
            "victors-phone"
            "sorlag"
          ];
        };
      };
    };
  };
}
