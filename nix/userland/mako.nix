{config, pkgs, lib, ...}:

{
  services.mako = {
    enable = true;
    settings = {
      anchor = "top-right";
      background-color = "#eff1f5";
      border-color = "#8caaee";
      border-radius = 5;
      height = 300;
      default-timeout = 5000;
      text-color = "#000000";
      font = "VictorMono Nerd Font 12";
      "category=sway.keymap" = {
        background-color="#E6E6FA";
        history=0;
        default-timeout=6000;
        height=1000;
        padding="10,20,20,10";
        text-color="#FF4500";
        width=500;
        anchor="center";
      };
    };
  };
}
