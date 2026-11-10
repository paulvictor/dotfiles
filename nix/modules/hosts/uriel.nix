# ThinkPad
{
  hosts.uriel = {
    profiles = [ "minimal" "graphical" "developer" "workstation" "laptop" ];

    nixos = {
      features = [ "arduino" ];
      stateVersion = "25.05";
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
      stateVersion = "24.11";
      modules = [{
        services.batteryAlert.enable = true;
      }];
    };
  };
}
