{ lib, pkgs, config, ... }:

lib.mkIf (config.networking.hostName == "uriel") {
  networking.firewall.allowedUDPPorts = [ config.services.tailscale.port ];
  networking.firewall.allowedTCPPorts = [ 5006 ];
  networking.nameservers = [ "100.100.100.100" "8.8.8.8" "1.1.1.1" ];
  environment.systemPackages = with pkgs; [ tailscale ];
  services.tailscale = {
    enable = true;
    port = 12345;
    authKeyFile = "/var/lib/tailscale/authkey";
    extraUpFlags = [
      "--operator"
      "viktor"
      "--ssh"
    ];
  };
  environment.persistence."/persist" = {
    directories = [
      "/var/lib/tailscale"
    ];
  };
}
