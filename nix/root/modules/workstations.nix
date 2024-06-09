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
    git
    htop
    man-pages
    neovim
    posix_man_pages
    psmisc
    wget
  ] ++ lib.optionals isPhysicalDevice [
    binutils
    bluez
    exfat
    file
    git
    gnupg
    home-manager
    kmonad
    libnotify
    nix-prefetch-github
    patchelf
    pciutils
    virt-manager
    wally-cli
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
      [ android-udev-rules yubikey-personalization ];
  services.pcscd.enable = isPhysicalDevice;
  programs.adb.enable = isPhysicalDevice;

  hardware.keyboard.zsa.enable = isPhysicalDevice;

  # For SSD's
  services.fstrim.enable = lib.mkForce isPhysicalDevice;

  # For laptops
  services.tlp.enable = lib.mkForce isPhysicalDevice;

  # Again, only for laptops
  # This will save you money and possibly your life!
  services.thermald.enable = lib.mkForce isPhysicalDevice;

  programs.firejail.enable = isPhysicalDevice;

  virtualisation.lxd.enable = false;

  security.wrappers = {
#     gllock = {
#       owner = "root";
#       group = "root";
#       setuid = true;
#       source = "${gllock}/bin/gllock";
#     };
    cryptsetup = {
      owner = "root";
      group = "root";
      source = "${cryptsetup}/bin/cryptsetup";
      capabilities = "cap_sys_admin+ep";
    };
  };

  imports =
    lib.optionals isPhysicalDevice
      [
        args.specialArgs.kmonad.nixosModules.default
        ./keyd.nix
      ];

}
