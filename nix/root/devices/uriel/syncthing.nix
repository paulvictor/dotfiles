{ config, ... } :

let
  syncthing-key-file = "/run/syncthing/key.pem";
  syncthing-cert-file = "/run/syncthing/cert.pem";
in
{
  imports = [
    ../../modules/syncthing.nix
  ];
  sops.defaultSopsFile = ./secrets/syncthing.yaml;
  sops.defaultSopsFormat = "yaml";
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  sops.secrets."syncthing/key.pem" = {
    owner = config.users.users.viktor.name;
    group = config.users.users.viktor.group;
#     mode = "O400";
    path = syncthing-key-file;
  };
  sops.secrets."syncthing/cert.pem" = {
    owner = config.users.users.viktor.name;
    group = config.users.users.viktor.group;
#     mode = "O400";
    path = syncthing-cert-file;
  };

  services.syncthing = {
    enable = true;
    user = "viktor";
    openDefaultPorts = true;
    configDir = "${config.users.users.viktor.home}/.config/syncthing";
    key = syncthing-key-file;
    cert = syncthing-cert-file;
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
          devices = [ "uriel" "victors-phone" ];
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
          ];
        };
      };
    };
  };
}
