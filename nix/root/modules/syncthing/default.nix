{config, ...}:

{
  imports = [
    ./devices.nix
    ./folders.nix
    ./secrets.nix
  ];

  services.syncthing = {
    user = config.me.username;
    openDefaultPorts = true;
    configDir = "${config.users.users.${config.me.username}.home}/.config/syncthing";
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
