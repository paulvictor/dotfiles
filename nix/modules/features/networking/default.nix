# NetworkManager, resolved, host blocklist
{
  flake.modules.nixos.networking = { config, lib, pkgs, ... }: {
    users.users.${config.me.username}.extraGroups = [ "networkmanager" ];


    networking.useDHCP = false;
    networking.enableIPv6 = false;

    boot.kernelParams = [ "ipv6.disable=1" ];

    networking.stevenblack = {
      enable = true;
      block = [
        "fakenews"
        "gambling"
        #       "social"
      ];
    };

    networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
    networking.networkmanager.enable = true;
    services.resolved = {
      enable = true;
    };
    networking.firewall.enable = lib.mkDefault false;
  };
}
