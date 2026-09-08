{ pkgs, ... }:

{
  hardware.keyboard.qmk.enable = true;
  environment.systemPackages = with pkgs; [via vial];
  services.udev.packages = with pkgs; [
    via
    vial
    qmk-udev-rules
    qmk qmk_hid
  ];
  users.users.viktor.extraGroups = [ "plugdev" ];
  services.udev.extraRules = ''
    # Match RMK / pid.codes Vendor ID
    KERNEL=="hidraw*", ATTRS{idVendor}=="1209", MODE="0660", GROUP="plugdev", TAG+="uaccess"

    # Match generic raw HID interfaces with Vial enabled
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{vi_enabled}=="1", MODE="0660", GROUP="plugdev", TAG+="uaccess"
  '';
}
