# Surface Pro 9
{ inputs, config, ... }: {
  hosts.slash = {
    profiles =
      with config.profiles; [
        minimal
        graphical
        developer
        workstation
        laptop
        work
      ];
    stateVersion = "25.11";

    nixos = {
      features = with config.features.nixos; [ keyboard-hacking arduino ];
      modules = [
        ./_slash-hardware.nix
        ({ lib, pkgs, ... }:
          let
            nixos-hardware-patched =
              inputs.nixpkgs.legacyPackages.x86_64-linux.applyPatches {
                name = "nixos-hardware-patched";
                src = inputs.nixos-hardware;
                patches = [ ../../local-patches/nixos-hardware-1870.patch ];
              };
          in
          {
            imports = [ "${nixos-hardware-patched}/microsoft/surface-pro/9" ];

            nix.extraOptions = ''
              !include /etc/nix/build-capacity.conf
            '';

            services.kanata.keyboards.builtin.devices = [ "/dev/input/by-path/platform-MSHW0343:00-event-kbd" ];

            powerManagement.enable = true;
            services.iptsd.enable = true;

            systemd.targets.hibernate.enable = false;
            systemd.targets.hybrid-sleep.enable = false;

            services.xserver.videoDrivers = [ "modesetting" ];
            hardware.graphics = {
              enable = true;
              extraPackages = with pkgs; [
                intel-media-driver
                intel-vaapi-driver
                libvdpau-va-gl
                intel-compute-runtime # Optional: adds OpenCL support for the Iris Xe GPU
              ];
            };

            services.tlp.enable = lib.mkForce false;

            hardware.microsoft-surface.kernelVersion = "stable";

            services.keyfreq = {
              enable = true;
              keyboardName = "kanata";
            };
          })
      ];
    };

    home = {
      modules = [{
        wayland.windowManager.sway.config.output."eDP-1".scale = "1.8";
        programs.waybar.settings.bottomBar.battery.bat = "BAT1";
        programs.waybar.settings.mainBar.battery.bat = "BAT1";
        services.batteryAlert.enable = true;
        services.kanshi.enable = true;
        custom.firefox.devPixelsPerPx = "1.50";
      }];
    };
  };
}
