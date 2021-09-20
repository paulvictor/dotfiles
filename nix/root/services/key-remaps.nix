{ lib, pkgs, config, ... }:

with pkgs;
let
  # Keycodes at https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h
  config = writeText "key-remap-config.yaml" ''
    TIMING:
      TAP_MILLISEC: 200
      DOUBLE_TAP_MILLISEC: 150

    MAPPINGS:
      - KEY: KEY_ENTER
        TAP: KEY_ENTER
        HOLD: KEY_RIGHTCTRL
  '';
  udevmon-config = writeText "udevmon.yaml" ''
   - JOB: "${interception-tools}/bin/intercept -g $DEVNODE | ${interception-tools-plugins.dual-function-keys}/bin/dual-function-keys -c ${config}  | ${interception-tools}/bin/uinput -d $DEVNODE"
     DEVICE:
       EVENTS:
         EV_KEY: [KEY_ENTER]
  '';
in
{
  config = {
    systemd.services.key-remaps = {
      wants = ["systemd-udev-settle.service"];
      after = ["systemd-udev-settle.service"];
      path = [ interception-tools interception-tools-plugins.dual-function-keys coreutils bash ];
      wantedBy = [ "multi-user.target" ] ;
      serviceConfig = {
        ExecStart = "${coreutils}/bin/nice -n -20 ${interception-tools}/bin/udevmon -c ${udevmon-config}";
      };
    };
  };
}
