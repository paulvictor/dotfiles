args@{ config, lib, pkgs, ... } :

let
  inherit (args) isPhysicalDevice;
  isCloudDevice = !isPhysicalDevice;
  inherit (args) system;
in
with pkgs;
{
  # Set your time zone.
  time.timeZone = "Asia/Kolkata";

  environment.systemPackages = [
    bc
    binutils
    file
    git
    gnupg
    guix
    home-manager
    htop
    libnl
    man-pages
    neovim
    nix-prefetch-github
    patchelf
    posix_man_pages
    psmisc
    wget
  ] ++ lib.optionals isPhysicalDevice [
    virt-manager
    libnotify
    bluez
    exfat
    pciutils
    wirelesstools
  ];

  services.upower.enable = isPhysicalDevice;
  #services.nixosManual.showManual = true;
  programs.zsh.enable = true;

  virtualisation.docker= {
    enable = isPhysicalDevice;
    logDriver = "journald";
  };
  services.udev.packages =
    lib.optionals isPhysicalDevice
      [ crda android-udev-rules yubikey-personalization ];
  services.pcscd.enable = isPhysicalDevice;
  programs.adb.enable = isPhysicalDevice;

  # For SSD's
  services.fstrim.enable = lib.mkForce isPhysicalDevice;

  # For laptops
  services.tlp.enable = lib.mkForce isPhysicalDevice;

  # Again, only for laptops
  # This will save you money and possibly your life!
  services.thermald.enable = lib.mkForce isPhysicalDevice;

  programs.firejail.enable = isPhysicalDevice;

  virtualisation.lxd.enable = isPhysicalDevice;

  security.wrappers = {
    gllock = {
      owner = "root";
      group = "root";
      setuid = true;
      source = "${gllock}/bin/gllock";
    };
    cryptsetup = {
      owner = "root";
      group = "root";
      source = "${cryptsetup}/bin/cryptsetup";
      capabilities = "cap_sys_admin+ep";
    };
  };
}
