{lib, config, pkgs, ...}:

{
  services.kanata = {
    enable = true;
    keyboards.builtin = {
      config = builtins.readFile ./${config.networking.hostName}.lisp;
      extraArgs = [ ];
      extraDefCfg = "
        process-unmapped-keys   yes
        concurrent-tap-hold     yes
        allow-hardware-repeat   yes
        log-layer-changes       no
        movemouse-inherit-accel-state yes
        movemouse-smooth-diagonals yes
      ";
      devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
    };
  };
}
