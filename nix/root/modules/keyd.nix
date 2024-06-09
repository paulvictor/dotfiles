args@{ pkgs, specialArgs, lib, config, ...}:

let
  allKeyboardsConfigs = import ./keyd-device-mappings.nix;
  thisDeviceConfig = allKeyboardsConfigs.${config.networking.hostName};
in
{
  environment.systemPackages = [ pkgs.keyd ];
  #systemd.services.keyd.wantedBy = lib.mkForce [];
  services.keyd = {

    enable = true;
    keyboards = thisDeviceConfig;
  };
}
