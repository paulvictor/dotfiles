{ config, lib, pkgs, ... } :

{

  networking.useDHCP = false;
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.
  networking.enableIPv6 = false;

  boot.kernelParams = [ "ipv6.disable=1" ];

  networking.stevenblack = {
    enable = true;
    block = ["fakenews" "gambling" "porn" "social"];
  };

  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
  networking.networkmanager.enable = true;
  networking.networkmanager.insertNameservers = [ "8.8.8.8" "8.8.4.4" ];

  networking.firewall.enable = lib.mkDefault false;

}
