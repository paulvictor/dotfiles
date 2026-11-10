# Machines that run on battery: power management and low battery warnings
{
  profiles.laptop = {
    nixos = [ "power" ];
    home = [ "battery-alert" ];
  };
}
