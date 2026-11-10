{ config, lib, pkgs, ... }:

{
  services.libinput.enable = true;
  hardware.graphics.enable = true;

  programs.chromium = {
    enable = true;
    extensions = [
      "ihlenndgcmojhcghmfjfneahoeklbjjh"
      "mhpeahbikehnfkfnmopaigggliclhmnc"
    ];
  };
  services.dbus.packages = [ pkgs.bluez ];

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  services.journald.settings.Journal = {
    RateLimitIntervalSec = "0";
    RateLimitBurst = 0;
  };

  programs.dconf.enable = true;
}
