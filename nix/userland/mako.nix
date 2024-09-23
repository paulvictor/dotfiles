{config, pkgs, lib, ...}:

{
  services.mako = {
    enable = true;
    anchor = "top-right";
    backgroundColor = "#eff1f5";
    borderColor = "#8caaee";
    borderRadius = 5;
    height = 300;
    defaultTimeout = 5000;
    textColor = "#000000";
    font = "VictorMono Nerd Font 12";

    extraConfig = ''
      [category=sway.keymap]
      background-color=#E6E6FA
      history=0
      max-history=0
      default-timeout=6000
      height=1000
      padding=10,20,20,10
      text-color=#FF4500
      width=500
      anchor=center
    '';
  };
}
