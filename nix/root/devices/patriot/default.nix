# Huh ? https://nixos.wiki/wiki/Adding_VMs_to_PATH
# TODO : Can it be started on system boot
{ config, lib, pkgs, modulesPath, ... }:

{
  system.stateVersion = "22.05"; # Did you read the comment?

  # From head -c4 /dev/urandom | od -A none -t x4
  networking.hostId =  "c0e166df";

  services.qemuGuest.enable = true;

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
    autoResize = true;
  };

  boot = {
    growPartition = true;
    kernelParams = [ "console=ttyS0" ];
    loader.grub.device = "/dev/vda";
    loader.timeout = 0;
  };

  virtualisation = {
    diskSize = 1024; # MB
    memorySize = 256; # MB
    writableStoreUseTmpfs = true;
    qemu.networkingOptions = [
#       "-nic user,model=virtio"
#       "-netdev user,id=user.0,net=192.168.76.0/24,dhcpstart=192.168.76.9"
#       "-net nic,netdev=user.0,model=virtio"
    ];
  };
  networking.firewall.enable = lib.mkForce false;

  environment.systemPackages = with pkgs;
    [ vim ];

  imports = [
    ../../modules/ssh.nix
  ];

}
