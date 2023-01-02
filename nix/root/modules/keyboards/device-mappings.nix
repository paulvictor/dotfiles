{
  sorlag = {
    microsoft-split-kbd = {
      device = "/dev/input/by-id/usb-Microsoft_Microsoft®_Nano_Transceiver_v2.1-event-kbd";
      defcfg.enable = true;
      config = builtins.readFile ./colemak-kmonad.lisp;
    };
  };
  uriel = {
    laptop-kbd = {
      device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
      defcfg.enable = true;
      config = builtins.readFile ./colemak-kmonad.lisp;
    };
  };
}

