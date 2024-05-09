{ config, lib, pkgs, ...}:

{
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    systemWide = true;
    alsa.enable = true;
    pulse.enable = true;
    wireplumber = {
      enable = true;
      configPackages = [
        pkgs.wireplumber
      ];
    };
  };
}
