{lib, config, pkgs, ...}:

{
  services.kanata = {
    enable = true;
    keyboards.bones = {
      config = builtins.readFile ./minimal.lisp;
      extraDefCfg = "
        process-unmapped-keys   yes
        concurrent-tap-hold     yes
        allow-hardware-repeat   no
      ";
      devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
    };
  };
}
