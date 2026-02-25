{ lib, pkgs, config, ... }:

{
  networking.firewall.allowedUDPPorts = [ config.services.tailscale.port ];
  networking.firewall.allowedTCPPorts = [ 5006 ];
  environment.systemPackages = with pkgs; [ tailscale ];
  services.tailscale = {
    enable = true;
    port = 12345;
    extraUpFlags = [
      "--operator" "viktor"
      "--ssh"
      "--accept-risk" "all"
      "--accept-routes"
      "--timeout" "10s"
      "--advertise-tags" "tag:workstations"
    ];
  };
}
