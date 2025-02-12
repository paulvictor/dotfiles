{ lib, pkgs, config, specialArgs, ... }:

lib.mkIf
  (config.sops.secrets ? "tailscale.authkey")
  {
    networking.firewall.allowedUDPPorts = [ config.services.tailscale.port ];
    networking.firewall.allowedTCPPorts = [ 5006 ];
    environment.systemPackages = with pkgs; [ tailscale ];
    services.tailscale = {
      enable = true;
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
