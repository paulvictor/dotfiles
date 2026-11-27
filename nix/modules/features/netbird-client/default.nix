# NetBird client: this machine joins the private network run by the netbird-server feature (host mynx).
#
# First login on each machine, once (opens the browser; the state survives reboots):
#   sudo netbird up
# or without a browser, with a setup key from the dashboard:
#   sudo netbird up --setup-key <key>
{ self, ... }:
let
  server = self.nixosConfigurations.mynx.config.services.netbird.server.domain;
in
{
  flake.modules.nixos.netbird-client = {
    # `services.netbird.enable` is the single default client: interface wt0, state in /var/lib/netbird
    services.netbird.enable = true;
    services.netbird.clients.default.environment.NB_MANAGEMENT_URL = "https://${server}";
  };
}
