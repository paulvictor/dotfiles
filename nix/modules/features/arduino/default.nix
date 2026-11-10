# Arduino toolchain access (udev rules, serial groups)
{
  flake.modules.nixos.arduino =
{config, lib, pkgs, ...}:

{
  users.users.${config.me.username}.extraGroups = [ "dialout" "tty" ];
  services.udev.packages = with pkgs; [ arduino-ide ];
  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ATTRS{idVendor}=="2341", ATTRS{idProduct}=="0069", MODE="0660", GROUP="plugdev"
    SUBSYSTEM=="usb", ATTRS{idVendor}=="2341", ATTRS{idProduct}=="0369", MODE="0660", GROUP="plugdev"
    # SUBSYSTEM=="usb", ATTRS{idVendor}=="2341", ATTRS{idProduct}=="0069", TAG+="uaccess"
    # Give dialout group access to Arduino / USB serial devices
    SUBSYSTEM=="tty", ATTRS{idVendor}=="2341", MODE="0660", GROUP="dialout"
    SUBSYSTEM=="tty", ATTRS{idVendor}=="1a86", MODE="0660", GROUP="dialout"
  '';

}
;

}
