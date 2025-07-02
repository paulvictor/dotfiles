{lib, config, pkgs, ...}:

{
  services.kanata = {
    enable = true;
    keyboards.bones = {
      config = builtins.readFile ./main.lisp;
      extraArgs = [ "--debug" ];
      extraDefCfg = "
        process-unmapped-keys   yes
        concurrent-tap-hold     yes
        allow-hardware-repeat   yes
      ";
      devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
    };
  };
}
