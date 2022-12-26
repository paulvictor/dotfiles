{ lib, pkgs, config, ... }:

with pkgs;
let
  # Keycodes at https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h
  config = writeText "key-remap-config.yaml" ''
    TIMING:
      TAP_MILLISEC: 200
      DOUBLE_TAP_MILLISEC: 150

    MAPPINGS:
      - KEY: KEY_SPACE
        TAP:  KEY_SPACE
        HOLD: KEY_LEFTALT
        HOLD_START: BEFORE_CONSUME
      - KEY: KEY_LEFTCTRL
        TAP:  [ KEY_LEFTCTRL, KEY_X ]
        HOLD: KEY_LEFTCTRL
        HOLD_START: BEFORE_CONSUME
      - KEY: KEY_RIGHTCTRL
        TAP:  [ KEY_LEFTALT, KEY_X ]
        HOLD: KEY_RIGHTCTRL
        HOLD_START: BEFORE_CONSUME
      - KEY: KEY_ENTER
        TAP: KEY_ENTER
        HOLD: KEY_RIGHTCTRL
        HOLD_START: BEFORE_CONSUME
      - KEY: KEY_LEFTSHIFT
        TAP: KEY_ESC
        HOLD: KEY_LEFTSHIFT
        HOLD_START: BEFORE_CONSUME
      - KEY: KEY_RIGHTSHIFT
        TAP: [KEY_LEFTCTRL, KEY_C ]
        HOLD: KEY_RIGHTSHIFT
        HOLD_START: BEFORE_CONSUME
      - KEY: KEY_CAPSLOCK
        TAP: [KEY_LEFTCTRL, KEY_LEFTMETA, KEY_LEFTALT, KEY_UP ]
        HOLD: [KEY_LEFTCTRL, KEY_LEFTMETA, KEY_LEFTALT ]
        HOLD_START: BEFORE_CONSUME
      # - KEY: KEY_TAB
      #   TAP: KEY_TAB
      #   HOLD: [KEY_LEFTCTRL, KEY_LEFTMETA, KEY_LEFTALT ]
      #   HOLD_START: BEFORE_CONSUME
  '';
  udevmon-config = writeText "udevmon.yaml" ''
   - JOB: "${interception-tools}/bin/intercept -g $DEVNODE | ${interception-tools-plugins.dual-function-keys}/bin/dual-function-keys -c ${config}  | ${interception-tools}/bin/uinput -d $DEVNODE"
     DEVICE:
       LINK: /dev/input/by-id/usb-Microsoft_Microsoft®_Nano_Transceiver_v2.1-event-kbd
       EVENTS:
         EV_KEY: [KEY_ENTER, KEY_LEFTCTRL, KEY_RIGHTCTRL, KEY_LEFTSHIFT, KEY_TAB, KEY_SPACE, KEY_RIGHTSHIFT, KEY_CAPSLOCK ]
  '';
in
{
  config = {
    systemd.services.key-remaps = {
      enable = true;
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
