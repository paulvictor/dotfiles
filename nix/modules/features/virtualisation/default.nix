# Containers and virtual machines: docker, virt-manager, and the groups that give the primary user access
{
  flake.modules.nixos.virtualisation = { config, lib, pkgs, ... }: {
    environment.systemPackages = [ pkgs.virt-manager ];

    virtualisation.docker = {
      enable = lib.mkDefault false;
      logDriver = "journald";
    };

    users.users.${config.me.username}.extraGroups = [
      "docker"
      "kvm"
      "libvirtd"
      "qemu-libvirtd"
      "lxd"
    ];
  };
}
