{ config, lib, pkgs, ... } :

{
  services.syncthing.devices = {
    sarge = {
      id = "OWYAVCE-HZNWJAH-CX4XEOR-ECZKTEB-W6YXQTB-B52HOXA-BRT7PBB-J3OQIQ7";
      name = "sarge";
      introducer = true;
    };
    uriel = {
      id = "YELHBGM-BI7EHRC-FOZ5GZS-VWT6BKO-IKFVAMP-XU5OC3C-ZYCDDVC-7RJUXAZ";
      name = "uriel";
      introducer = true;
    };
  };
}
