# Physical machines: firmware
{
  flake.modules.nixos.hardware = { lib, ... }: {
    hardware.enableRedistributableFirmware = lib.mkDefault true;
  };
}
