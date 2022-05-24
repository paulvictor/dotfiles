{ config, lib, pkgs, specialArgs, ... } :

lib.attrsets.optionalAttrs
  specialArgs.isPhysicalDevice
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
      hostsFile = pkgs.callPackage ./mk-adblock-list.nix {};
    in builtins.readFile "${hostsFile}";
    networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
    networking.networkmanager.enable = true;
    networking.networkmanager.insertNameservers = [ "8.8.8.8" "8.8.4.4" ];
    networking.firewall.enable = lib.mkDefault false;

    services.openssh = {
      enable = true;
      forwardX11 = true;
      authorizedKeysCommand = with pkgs;
        let
          script =
            writeShellScript
              "getMyKey"
              "${curl}/bin/curl -L https://github.com/paulvictor.keys";
        in "${script}";
      hostKeys =
        lib.optionals specialArgs.isPhysicalDevice [
          {
            path = "/tomb/${config.networking.hostName}/ssh/ssh_host_rsa_key";
            type = "rsa";
            bits = 4096;
          }
        ];
    };
  }
