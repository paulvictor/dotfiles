{ config, lib, pkgs, specialArgs, modulesPath, ...}:

{
  # Allow packet forwarding
  # For wireguard
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
  };

  networking.firewall = {
    enable = lib.mkForce true;
    allowedTCPPorts = [ 22 443 5600 4443 ];
  };

  boot.growPartition = true;
#   boot.loader.grub.device = "/dev/vda";
#   boot.loader.timeout = 0;

  # From head -c4 /dev/urandom | od -A none -t x4
  networking.hostId = "745e2308";

  services.cloud-init = {
    enable = true;
    network.enable = true;
    btrfs.enable = true;
    ext4.enable = true;
  };

  services.actual-server.enable = true;

  imports = [
#     "${toString modulesPath}/profiles/headless.nix"
    ../../modules/viktor.nix
    ../../modules/workstations.nix
  ];

  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.man.enable = false;
  documentation.info.enable = false;
  services.ssm-agent = {
    enable = true;
    package = pkgs.ssm-agent;
  };
}
