# The droplet, booted from the custom image uploaded from
# `nix build .#nixosConfigurations.mynx.config.system.build.digitalOceanImage`
{ config, ... }: {
  data.digitalocean_image.mynx = {
    name = "mynx-v1";
    source = "user";
  };

  data.digitalocean_ssh_key = {
    global.name = "global";
  };

  resource.digitalocean_droplet.mynx = {
    name = "mynx";
    region = "blr1";
    size = "s-1vcpu-2gb"; # $12: 1 vCPU, 2 GB, 50 GB
    image = config.data.digitalocean_image.mynx "id";
    ssh_keys = [ (config.data.digitalocean_ssh_key.global "id") ];
    ipv6 = false;
    backups = false;
    # What runs on it comes from deploy-rs, not from the image or boot-time data
    lifecycle = {
      prevent_destroy = true;
      ignore_changes = [ "image" "ssh_keys" "user_data" ];
    };
  };

  output.mynx_ipv4.value = config.resource.digitalocean_droplet.mynx "ipv4_address";
}
