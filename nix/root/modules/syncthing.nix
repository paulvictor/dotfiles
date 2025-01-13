{ config, lib, pkgs, ... } :

{
  services.syncthing = {
    settings = {
      devices = {
        uriel = {
          id = "6XM6BA2-3ROYNBY-J4LVX5D-3NWQ7CR-NUE237C-C7XKZNZ-BFTI2KJ-E2SWGQC";
          autoAcceptFolders = true;
        };
        victors-phone = {
          id = "UBULWV6-3KYFWTK-PMIYLWK-ZXY6IRE-CDTRXLV-3YFCURL-QAZMSPQ-G46OSAH";
          autoAcceptFolders = true;
        };
      };
    };
  };
  systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true"; # Don't create default ~/Sync folder
}
