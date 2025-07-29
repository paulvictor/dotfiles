{ config, lib, pkgs, ... } :

{
  services.syncthing = {
    settings = {
      devices = {
        sarge = {
          id = "FHS2LGI-LTCSU2F-NKKI3TB-RVOG3FM-IVDPPHI-DD74X6W-MKIQ3GG-KBNWTQO";
          autoAcceptFolders = true;
        };
        uriel = {
          id = "6XM6BA2-3ROYNBY-J4LVX5D-3NWQ7CR-NUE237C-C7XKZNZ-BFTI2KJ-E2SWGQC";
          autoAcceptFolders = true;
        };
        victors-phone = {
          id = "UBULWV6-3KYFWTK-PMIYLWK-ZXY6IRE-CDTRXLV-3YFCURL-QAZMSPQ-G46OSAH";
          autoAcceptFolders = true;
        };
        sorlag = {
          id = "GDNPOFY-DFQENWZ-ETUZTLL-ST6JG2W-VCJ4FIZ-WRYRJNY-XPC5CUW-JVQ6ZQE";
          autoAcceptFolders = true;
        };
      };
    };
  };
  systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true"; # Don't create default ~/Sync folder
}
