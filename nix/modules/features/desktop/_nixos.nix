{ config, lib, pkgs, ... }:

{
  services.libinput.enable = true;

  # keep timezone updated to local time using geoclue
  time.timeZone = null;
  services.localtimed.enable = true;
  services.geoclue2.enable = true;

  environment.systemPackages = with pkgs; [
    bluez
    libnotify
  ];

  programs.firejail.enable = true;

  users.users.${config.me.username}.extraGroups = [ "video" ]; # backlight
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
