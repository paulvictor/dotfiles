{ config, lib, pkgs, specialArgs, ... } :

{

  networking.useDHCP = false;
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.
  networking.enableIPv6 = false;

  networking.extraHosts = let
    hostsFile = pkgs.callPackage ./mk-adblock-list.nix {};
  in builtins.readFile "${hostsFile}";

  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
  networking.networkmanager.enable = true;
  networking.networkmanager.insertNameservers = [ "8.8.8.8" "8.8.4.4" ];

  networking.firewall.enable = lib.mkDefault false;

}
