# ThinkPad
{ config, ... }: {
  hosts.uriel = {
    profiles = with config.profiles; [ minimal graphical developer workstation laptop ];
    stateVersion = "25.05";

    nixos = {
      features = with config.features.nixos; [ arduino ];
      modules = [
        ./_uriel-hardware.nix
        ({ pkgs, ... }: {
          networking.hostId = "237ab499";
          boot.kernelPackages = pkgs.linuxPackages_latest;
          boot.loader.systemd-boot.enable = true;
          boot.loader.efi.canTouchEfiVariables = true;
        })
      ];
    };

    home = {
      modules = [{
        services.batteryAlert.enable = true;
      }];
    };
  };
}
