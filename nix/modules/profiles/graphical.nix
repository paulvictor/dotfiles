# A usable graphical desktop on a physical machine: sound, fonts, network, a window manager, browser, terminal
{ config, ... }: {
  profiles.graphical = {
    nixos = with config.features.nixos; [ hardware networking fonts audio desktop sway ];
    home = with config.features.home; [ fonts audio desktop terminal rofi sway firefox ];
  };
}
