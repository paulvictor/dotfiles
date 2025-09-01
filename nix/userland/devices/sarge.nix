{lib,...}:
{
  services.batteryAlert.enable = false;
  wayland.windowManager.sway.config.output = {
    "HDMI-A-1" = {
      mode = "2560x1440@60Hz";
      transform = "270";
    };
  };
}
