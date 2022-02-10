{ config, lib, pkgs, ... } :

{
  services.syncthing.devices = {
    sarge = {
      id = "OWYAVCE-HZNWJAH-CX4XEOR-ECZKTEB-W6YXQTB-B52HOXA-BRT7PBB-J3OQIQ7";
      name = "sarge";
      introducer = true;
    };
    uriel = {
      id = "IYLKTPE-SF5YCW4-XXC6C5H-JJF6IZL-NMXDFL5-G2JDDBE-3QS4GSD-I4J3IAC";
      name = "uriel";
      introducer = true;
    };
  };
}
