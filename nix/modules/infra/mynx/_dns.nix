# DNS only (grey cloud): the Cloudflare proxy would break the UDP relay and complicates gRPC.
# The machine has an A record; the names of the services on it are CNAMEs to it, so a new IP changes one record.
{ config, hostname, domain, zone, ... }: {
  data.cloudflare_zone.main.filter.name = zone;

  resource.cloudflare_dns_record.mynx = {
    zone_id = config.data.cloudflare_zone.main "id";
    name = hostname;
    type = "A";
    content = config.resource.digitalocean_droplet.mynx "ipv4_address";
    ttl = 300;
    proxied = false;
    comment = "mynx droplet (terranix)";
  };

  resource.cloudflare_dns_record.netbird = {
    zone_id = config.data.cloudflare_zone.main "id";
    name = domain;
    type = "CNAME";
    content = hostname;
    ttl = 300;
    proxied = false;
    comment = "NetBird server (terranix)";
  };
}
