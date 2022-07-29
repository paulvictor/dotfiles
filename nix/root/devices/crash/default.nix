{ config, lib, pkgs, modulesPath, ...}:

{
  system.stateVersion = "22.05"; # Did you read the comment?

  # Allow packet forwarding
  # For wireguard
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
  };

  networking.firewall = {
    enable = lib.mkForce true;
    allowedUDPPorts = [ 53 9228 3754 ];
    allowedTCPPorts = [ 22 443 8080 4443 ];
  };

  boot.growPartition = true;
#   boot.loader.grub.device = "/dev/vda";
#   boot.loader.timeout = 0;

  # From head -c4 /dev/urandom | od -A none -t x4
  networking.hostId = "ef9e199a";

  services.cloud-init = {
    enable = true;
    network.enable = true;
    btrfs.enable = true;
    ext4.enable = true;
  };

  imports = [
#     "${toString modulesPath}/profiles/headless.nix"
    ../../modules/viktor.nix
    ../../modules/workstations.nix
  ];

}
