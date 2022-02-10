{ config, lib, pkgs, ... } :

{
  environment.etc."NetworkManager/system-connections" = {
    source = "/persist/etc/NetworkManager/system-connections/";
  };
  systemd.tmpfiles.rules = [
    "L /var/lib/bluetooth - - - - /persist/var/lib/bluetooth"
  ];

  services.zfs.autoScrub.enable = true;
  boot.loader.grub.copyKernels = true;
  boot.initrd.postDeviceCommands = pkgs.lib.mkAfter ''
    zfs rollback -r master/local/root@blank
  '';

}
