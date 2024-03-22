{ config, lib, pkgs, ...}:

{
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    systemWide = true;
    alsa.enable = true;
    pulse.enable = true;
  };
  services.pipewire.wireplumber = {
    enable = true;
    configPackages = [
      pkgs.wireplumber
    ];
  };
}
