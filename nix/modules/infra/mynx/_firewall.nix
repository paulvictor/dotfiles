# The same ports as the NixOS firewall of mynx, as a second layer in front of it
{ config, host, ... }:
let
  firewall = host.networking.firewall;
  everywhere = [ "0.0.0.0/0" "::/0" ];
  range = r: "${toString r.from}-${toString r.to}";
  inbound = protocol: ports: ranges:
    map (p: { inherit protocol; port_range = toString p; source_addresses = everywhere; }) ports
    ++ map (r: { inherit protocol; port_range = range r; source_addresses = everywhere; }) ranges;
in
{
  resource.digitalocean_firewall.mynx = {
    name = "mynx";
    droplet_ids = [ (config.resource.digitalocean_droplet.mynx "id") ];
    inbound_rule =
      inbound "tcp" firewall.allowedTCPPorts firewall.allowedTCPPortRanges
      ++ inbound "udp" firewall.allowedUDPPorts firewall.allowedUDPPortRanges
      ++ [{ protocol = "icmp"; source_addresses = everywhere; }];
    outbound_rule = [
      { protocol = "tcp"; port_range = "1-65535"; destination_addresses = everywhere; }
      { protocol = "udp"; port_range = "1-65535"; destination_addresses = everywhere; }
      { protocol = "icmp"; destination_addresses = everywhere; }
    ];
  };
}
