{ config, lib, pkgs, ... } :

{
  networking.useDHCP = false;
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.
  networking.enableIPv6 = false;
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  networking.extraHosts = let
    hostsFile = builtins.fetchurl {
      url = https://raw.githubusercontent.com/jerryn70/GoodbyeAds/a780eb7563af945b3364d5fcbca37dcafbb26da8/Hosts/GoodbyeAds.txt;
      sha256 = "15d3w6sp7xfsbala1rbhgk6y9hv2f6cbpswhnrg6sqfffi6gfrdi";
    };
  in hostsFile;
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
  networking.networkmanager.insertNameservers = [ "8.8.8.8" "8.8.4.4" ];
  networking.firewall.enable = false;

  services.openssh = {
    enable = true;
    forwardX11 = true;
    hostKeys = [
      {
        path = "/tomb/${config.networking.hostName}/ssh/ssh_host_rsa_key";
        type = "rsa";
        bits = 4096;
      }
    ];
  };
  networking.networkmanager.enable = true;
}
