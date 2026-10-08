{
  sarge = {
    additionalModules = [
      ({lib,...}:{
        home.stateVersion = "25.05";
        wayland.windowManager.sway.config.output = lib.mkForce {
          "HDMI-A-1" = {
            mode = "2560x1440@60Hz";
          };
        };
      })
    ];
  };
  slash = {
    additionalModules = [
      ({
        home.stateVersion = "25.11";
        wayland.windowManager.sway.config.output."eDP-1".scale = "1.8";
        programs.waybar.settings.bottomBar.battery.bat = "BAT1";
        programs.waybar.settings.mainBar.battery.bat = "BAT1";
        services.batteryAlert.enable = true;
        services.kanshi.enable = true;
      })
    ];
  };
  anarki = {
    additionalModules = [
      ({
        home.stateVersion = "25.11";
        programs.alacritty.settings.font.size = 16.0;
      })
    ];
  };
  uriel = {
    additionalModules = [
      ({
        home.stateVersion = "24.11";
        services.batteryAlert.enable = true;
      })
    ];
  };
  sorlag = {
    additionalModules = [
      ({lib,...}:{
        home.stateVersion = "25.11";
        services.batteryAlert.enable = true;
        services.kanshi.enable = true;
      })
    ];
  };
  bones = {
    additionalModules = [
      ({lib,...}:{
        home.stateVersion = "25.05";
        services.batteryAlert.enable = true;
        wayland.windowManager.sway.config.output = lib.mkForce {
          "DSI-1" = { # TODO, can we do only on bones.
            mode = "1920x1200@60Hz";
            pos = "0 0";
            transform = "90";
            scale = "1.60";
          };
        };
      })
    ];
  };
  # Darwin machine, not in use
  # "paul.victor@crash" = {
  #   isDesktop = false; # Desktop environment setup. Roughly if any of the X related things should be enabled
  #   additionalModules = [
  #     {
  #       home.username = "paul.victor";
  #       home.homeDirectory = "/Users/paul.victor";
  #       home.stateVersion = "25.05";
  #       home.sessionPath = [ "/run/current-system/sw/bin" ];
  #     }
  #   ];
  # };
}
