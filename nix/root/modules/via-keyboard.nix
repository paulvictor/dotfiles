{pkgs, ...}:

{
   environment.systemPackages = with pkgs; [
    via
    qmk
    qmk-udev-rules
  ];
  services.udev.packages = with pkgs;[
    via
    qmk-udev-rules
  ];
}
