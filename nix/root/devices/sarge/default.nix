# TODO : Look at https://github.com/openlab-aux/vuizvui for a better arrangement
# { config, pkgs, ... }:

{
  networking.hostName = "sarge";
  # From head -c4 /dev/urandom | od -A none -t x4
  networking.hostId = "d11572e4";
  imports = [ ./hardware-configuration.nix ];
  boot.loader.systemd-boot.enable = false;
  boot.loader.efi.efiSysMountPoint = "/boot";
  #boot.initrd.postDeviceCommands = pkgs.lib.mkAfter ''
  #  zfs rollback -r master/local/root@blank
  #'';
}
