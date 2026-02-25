{
  "viktor@sarge" = {
    additionalModules = [
      ({lib,...}:{
        home.stateVersion = "25.05";
        services.batteryAlert.enable = false;
        wayland.windowManager.sway.config.output = lib.mkForce {
          "HDMI-A-1" = {
            mode = "2560x1440@60Hz";
          };
        };
      })
    ];
  };
  "viktor@anarki" = {
    additionalModules = [
      ({
        home.stateVersion = "25.11";
        services.batteryAlert.enable = false;
      })
    ];
  };
  "viktor@uriel" = {
    additionalModules = [
      ({
        home.stateVersion = "24.11";
        services.batteryAlert.enable = true;
      })
    ];
  };
  "viktor@sorlag" = {
    additionalModules = [
      ({lib,...}:{
        home.stateVersion = "25.11";
        services.batteryAlert.enable = true;
      })
    ];
  };
  "viktor@bones" = {
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
  "paul.victor@crash" = {
    isDesktop = false; # Desktop environment setup. Roughly if any of the X related things should be enabled
    additionalModules = [
      {
        home.username = "paul.victor";
        home.homeDirectory = "/Users/paul.victor";
        home.stateVersion = "25.05";
        home.sessionPath = [ "/run/current-system/sw/bin" ];
      }
    ];
  };
}
