{
  uriel = {
    laptop-kbd = {
      device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
      defcfg.enable = true;
      config = builtins.readFile ./colemak-kmonad.lisp;
    };
  };
}

