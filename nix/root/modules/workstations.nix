args@{ config, lib, pkgs, ... } :

let
  inherit (args) isPhysicalDevice;

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
    man-pages-posix
    manix
    psmisc
    wget
    fup-repl
  ] ++ lib.optionals isPhysicalDevice [
    binutils
    bluez
    exfat
    file
    git
    gnupg
    home-manager
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
#   security.polkit.enable = lib.mkForce false;

  services.pcscd.enable = isPhysicalDevice;
  users.groups.ykusers = { };
  security.polkit.extraConfig = ''
    polkit.addRule(function (action, subject) {
      if (
        (action.id == "org.debian.pcsc-lite.access_pcsc" ||
          action.id == "org.debian.pcsc-lite.access_card") &&
         subject.isInGroup("ykusers")
      ) {
        return polkit.Result.YES;
      }
    });
  '';
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
    cryptsetup = {
      owner = "root";
      group = "root";
      source = "${cryptsetup}/bin/cryptsetup";
      capabilities = "cap_sys_admin+ep";
    };
  };

  imports =
      [
        ./keyd.nix
        ../../root/tailscale.nix
      ];

}
