# Desktop with an NVIDIA GPU, runs local models and is the remote builder
{ config, ... }: {
  hosts.anarki = {
    profiles = with config.profiles; [
      minimal
      graphical
      developer
      workstation
      work
    ];
    stateVersion = "25.11";

    nixos = {
      features = with config.features.nixos; [ local-ai-server keyboard-hacking remote-builder arduino ];
      modules = [
        ./_anarki-hardware.nix
        ({ pkgs, ... }: {
          powerManagement.enable = true;

          boot.loader.systemd-boot.enable = true;
          boot.loader.efi.canTouchEfiVariables = true;

          boot.kernelPackages = pkgs.linuxPackages_6_18;
        })
      ];
    };

    home = {
      modules = [{
        programs.alacritty.settings.font.size = 16.0;
        custom.firefox.devPixelsPerPx = "1.50";
        custom.sway.displayOffOnIdle = false; # keep the monitor on
      }];
    };
  };
}
