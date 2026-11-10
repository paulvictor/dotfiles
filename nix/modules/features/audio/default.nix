# Sound: system-wide PipeWire, and a volume control for the user
{
  flake.modules.nixos.audio =
{ config, lib, pkgs, ...}:

{
  users.users.${config.me.username}.extraGroups = [ "audio" "pipewire" ];

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
;

  flake.modules.homeManager.audio = { pkgs, ... }: {
    home.packages = [ pkgs.pavucontrol ];
  };
}
