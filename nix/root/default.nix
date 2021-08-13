let
  # This function takes the path of a device module as an argument
  # and returns a complete module to be imported in configuration.nix
  makeDevice = devicePath:
    { config, pkgs, lib, ... }:
      {
        imports = [
          ./common-config.nix
          devicePath
        ];
      };
  deviceModules =
    builtins.listToAttrs (map (deviceName: {
      name = deviceName;
      value = makeDevice (./devices + "/${deviceName}");
    }) (import ./devices/all-devices.nix));
in
deviceModules
