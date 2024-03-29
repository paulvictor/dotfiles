{ config, lib, pkgs, specialArgs, modulesPath, ...}:

{
  # Allow packet forwarding
  # For wireguard
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
  };

  networking.firewall = {
    enable = lib.mkForce true;
    allowedTCPPorts = [ 22 80 443 5600 4443 ];
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
    ./proxy-to-actual.nix
  ];

  documentation.enable = false;
  documentation.doc.enable = false;
  documentation.man.enable = false;
  documentation.info.enable = false;
  services.ssm-agent = {
    enable = true;
    package = pkgs.ssm-agent;
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "paulvictor@gmail.com";
    certs."paulvictor.xyz" = {
      dnsProvider = "googledomains";
      extraDomainNames = "accounts.paulvictor.xyz";
      # Check https://go-acme.github.io/lego/dns/googledomains/ on how to set this up
      credentialsFile = "/var/lib/acme/paulvictor.xyz/google-domains.creds";
      webroot = null;
      email = "paulvictor@gmail.com";
      group = "nginx";
    };
  };

  services.nginx = {
    enable = true;
    virtualHosts = {
      "paulvictor.xyz" = {
        forceSSL = true;
        enableACME = true;
      };
    };
  };

  # /var/lib/acme/.challenges must be writable by the ACME user
  # and readable by the Nginx user. The easiest way to achieve
  # this is to add the Nginx user to the ACME group.
  users.users.nginx.extraGroups = [ "acme" ];
  users.users.viktor.extraGroups = [ "acme" "nginx" ];

}
