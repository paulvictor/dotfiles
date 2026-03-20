{config, ...}:

{
  imports = [
    ./devices.nix
    ./folders.nix
    ./secrets.nix
  ];

  services.syncthing = {
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
    };
  };
  systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true";# Don't create default ~/Sync folder

}
