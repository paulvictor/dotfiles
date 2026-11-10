# A usable graphical desktop: sound, fonts, network, a window manager, browser, terminal
{
  profiles.graphical = {
    nixos = [ "basics" "networking" "fonts" "audio" "desktop" "sway" ];
    home = [ "fonts" "audio" "desktop" "terminal" "rofi" "sway" "firefox" ];
  };
}
