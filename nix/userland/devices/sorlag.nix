{lib,...}:
{
  services.batteryAlert.enable = false;
  wayland.windowManager.sway.config.output = {
    "HDMI-A-1" = lib.mkForce {
      mode = "2560x1440@60Hz";
    };
  };
};
