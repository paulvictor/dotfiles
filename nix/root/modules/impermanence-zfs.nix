{ config, lib, pkgs, ... } :

{
  environment.etc."NetworkManager/system-connections" = {
    source = "/persist/etc/NetworkManager/system-connections/";
  };
  systemd.tmpfiles.rules = [
#     "C /var/lib/bluetooth - - - - /persist/var/lib/bluetooth"
#     "d /var/lib/bluetooth 700 root root -"
#     "L /var/lib/bluetooth - - - - /persist/var/lib/bluetooth"
  ];
  systemd.targets."bluetooth".after = ["systemd-tmpfiles-setup.service"];

  services.zfs.autoScrub.enable = true;
  boot.loader.grub.copyKernels = true;
  boot.initrd.postDeviceCommands = pkgs.lib.mkAfter ''
    zfs rollback -r master/local/root@blank
  '';

}
