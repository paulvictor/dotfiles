{config, ...}:

let
  devices = [ "uriel" "victors-phone" "sarge" "sorlag" ];
in
{
  services.syncthing.settings.folders = {
    photos = {
      id = "bmce4-0zr5d";
      label = "Photos On Victors Pixel 8";
      inherit devices;
      path = "${config.users.users.viktor.home}/Photos/victor-pixel8-photos";
      type = "receiveonly";
      copyOwnershipFromParent = true;
    };
    roam-notes = {
      id = "roam-notes";
      label = "Org roam notes";
      path = "${config.users.users.viktor.home}/org-roam-notes";
      copyOwnershipFromParent = true;
      versioning = {
        type = "simple";
        params.keep = "10";
      };
      inherit devices;
    };
  };
}
