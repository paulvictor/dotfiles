# Machines that run on battery: power management and low battery warnings
{ config, ... }: {
  profiles.laptop = {
    nixos = with config.features.nixos; [ power ];
    home = with config.features.home; [ battery-alert ];
  };
}
