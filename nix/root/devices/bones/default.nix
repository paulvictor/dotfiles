{ config, lib, pkgs, specialArgs, modulesPath, ...}:

{
  system.stateVersion = "22.05"; # Did you read the comment?

  # Set in devices/default.nix
#   networking.hostName = "bones";

  # Allow packet forwarding
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
  };

  networking.firewall = {
    enable = lib.mkForce true;
    allowedUDPPorts = [ 22 53 9228 3754 ];
    allowedTCPPorts = [ 8080 4443 ];
  };

  services.openssh = {
    enable = true;
    permitRootLogin = "yes";
    gatewayPorts = "yes";
  };

  boot.growPartition = true;
  boot.loader.grub.device = "/dev/vda";
  boot.loader.timeout = 0;

  # From head -c4 /dev/urandom | od -A none -t x4
  networking.hostId = "00cfadc4";
#   boot.loader.systemd-boot.enable = true;
#   boot.loader.efi.efiSysMountPoint = "/boot";
  boot.kernelPackages = pkgs.linuxPackages_5_15;

  services.cloud-init = {
    enable = true;
    network.enable = true;
    btrfs.enable = true;
    ext4.enable = true;
#     config =
  };


  imports = [
    "${toString modulesPath}/profiles/headless.nix"
    ../../modules/viktor.nix
    ../../modules/workstations.nix
  ] ++ lib.optionals specialArgs.isPhysicalDevice [
    ./hardware-configuration.nix
    ./syncthing.nix
    ../../modules/desktop-environment.nix
    ../../modules/impermanence-zfs.nix
    ../../modules/networking.nix
  ];
}
