args@{ pkgs, specialArgs, lib, config, ...}:

let
  allKeyboardsConfigs = import ./keyd-device-mappings.nix;
  thisDeviceConfig = allKeyboardsConfigs.${config.networking.hostName} or null;
in
{
  environment.systemPackages = [ pkgs.keyd ];
  #systemd.services.keyd.wantedBy = lib.mkForce [];
  services.keyd = {

    enable = !(isNull thisDeviceConfig);
    keyboards = thisDeviceConfig;
  };
}
