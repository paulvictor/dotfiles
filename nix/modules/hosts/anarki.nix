# Desktop with an NVIDIA GPU, runs local models and is the remote builder
{
  hosts.anarki = {
    profiles = [ "minimal" "graphical" "developer" "workstation" "work" ];

    nixos = {
      features = [ "local-ai-server" "keyboard-hacking" "remote-builder" "arduino" ];
      stateVersion = "25.11";
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
      stateVersion = "25.11";
      modules = [{
        programs.alacritty.settings.font.size = 16.0;
        custom.firefox.devPixelsPerPx = "1.50";
        custom.sway.displayOffOnIdle = false; # keep the monitor on
      }];
    };
  };
}
