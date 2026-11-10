# Chuwi MiniBook X
{ inputs, config, ... }: {
  hosts.bones = {
    profiles = with config.profiles; [ minimal graphical developer workstation laptop ];
    stateVersion = "25.05";

    nixos = {
      features = with config.features.nixos; [ arduino ];
      modules = [
        ./_bones-hardware.nix
        inputs.nixos-hardware.nixosModules.chuwi-minibook-x
        {
          services.kanata.keyboards.builtin.devices = [ "/dev/input/by-path/platform-i8042-serio-0-event-kbd" ];

          boot.loader.systemd-boot.enable = true;
          boot.loader.efi.canTouchEfiVariables = true;
        }
      ];
    };

    home = {
      modules = [
        ({ lib, ... }: {
          services.batteryAlert.enable = true;
          wayland.windowManager.sway.config.output = lib.mkForce {
            "DSI-1" = {
              mode = "1920x1200@60Hz";
              pos = "0 0";
              transform = "90";
              scale = "1.60";
            };
          };
        })
      ];
    };
  };
}
