{ lib, pkgs, config, specialArgs, ... }:

{
  networking.firewall.allowedUDPPorts = [ config.services.tailscale.port ];
  networking.firewall.allowedTCPPorts = [ 5006 ];
  networking.nameservers = [ "100.100.100.100" "8.8.8.8" "1.1.1.1" ];
  environment.systemPackages = with pkgs; [ tailscale ];
  services.tailscale = {
    enable = (config.sops.secrets ? "tailscale.authkey");
    port = 12345;
    authKeyFile = config.sops.secrets."tailscale.authkey".path;
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
