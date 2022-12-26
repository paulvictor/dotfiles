args@{ pkgs, specialArgs, lib, ...}:

{
  services.kmonad = {
    enable = false;
    package = specialArgs.kmonad.packages.${specialArgs.system}.kmonad;
    keyboards.microsoft-split-kdb = {
      device = "/dev/input/by-id/usb-Microsoft_Microsoft®_Nano_Transceiver_v2.1-event-kbd";
      defcfg.enable = true;
      config = builtins.readFile ./colemak-kmonad.lisp;
    };
  };
}
