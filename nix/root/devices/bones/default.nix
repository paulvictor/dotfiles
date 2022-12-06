{ config, lib, pkgs, specialArgs, modulesPath, ...}:

{
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
  boot.loader.grub.device = "/dev/vda";
  boot.loader.timeout = 0;

  # From head -c4 /dev/urandom | od -A none -t x4
  networking.hostId = "00cfadc4";

  services.cloud-init = {
    enable = true;
    network.enable = true;
    btrfs.enable = true;
    ext4.enable = true;
  };

  imports = [
    "${toString modulesPath}/profiles/headless.nix"
    ../../modules/viktor.nix
    ../../modules/workstations.nix
  ];

  users.users.viktor.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDqO0CjHsgV+Vqhy0j5UzABRHEneZskMcMkiBlYFn14TZAYAHcxO8vBtbXhuLMoELj6P0uk64X15KceaxLCqDx5fLzKnbZ9Z++bMxEUBah/GagnJiirO1MUPS1vIIiYOjx2VgHg0a8e5qKIzs2RTQL12AgyHpbz2268HuymMykyaOyMVY5Ns9ZK0x9QRQ4uwNKHL4rhYG7U7vpEh78q+XSu3pD0cZp4Ih21d75YyLxSgpDNa65xTl/1IbvKTfZCztrlUnXFhByrq+pc+64kzITE1gLXCdhJfELl2MXrB1545YjMLC/T9YDt8MS1eH30NmgCtTbiQFA58s0WIHAwM6FG4vUfqEMPh0pbMAkSs95a1/meM/hefik4do1jT88QDiiHYrmp6mjx4Mabtwy5ks5K4YvWD6wicprLhLCWYSIMLuLVsQFN/zgfrWbRyEq5/dv7Yg1SX5VngbcAkQ6bdtFKwGodFwxTxHBgEfLgMRnGkBZiZwToUHldsxhxnJsIc68= paul@Ps-Mac-mini.local"
  ];
}
