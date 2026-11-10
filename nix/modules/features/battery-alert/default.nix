# Desktop notification on low battery; hosts enable it with services.batteryAlert.enable
{
  flake.modules.homeManager.battery-alert = ./_service.nix;
}
