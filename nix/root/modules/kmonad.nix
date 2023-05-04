args@{ pkgs, specialArgs, lib, ...}:

let
  allKeyboardsConfigs = import ./keyboards/device-mappings.nix;
  thisDeviceConfig = allKeyboardsConfigs.${specialArgs.hostName};
in
{
  services.kmonad = {
    enable = true;
    package = specialArgs.kmonad.packages.${specialArgs.system}.kmonad;
    keyboards = thisDeviceConfig;
    extraArgs = [ "--log-level" "debug" ];
  };
}
