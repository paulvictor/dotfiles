{ config, lib, pkgs, ... } :


{
  # Set your time zone.
  time.timeZone = "Asia/Kolkata";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  environment.systemPackages = with pkgs; [
    bc
    binutils
    bluez
    exfat
    file
    git
    gnupg
    guix
    home-manager
    htop
    libnl
    libnotify
    man-pages
    posix_man_pages
    nix-prefetch-github
    patchelf
    pciutils
    psmisc
    vim
    wget
    wirelesstools
  ];


  services.upower.enable = true;
  #services.nixosManual.showManual = true;
  programs.zsh.enable = true;

  virtualisation.docker= {
    enable = true;
    logDriver = "journald";
  };
  services.udev.packages =
    with pkgs; [ crda android-udev-rules yubikey-personalization ];
  services.pcscd.enable = true;
  programs.adb.enable = true;

  # For SSD's
  services.fstrim.enable = pkgs.lib.mkDefault true;

  # For laptops
  services.tlp.enable = pkgs.lib.mkDefault true;

  # Again, only for laptops
  # This will save you money and possibly your life!
  services.thermald.enable = pkgs.lib.mkDefault true;

  programs.firejail.enable = true;

  virtualisation.lxd.enable = true;

  security.wrappers = {
    gllock = {
      owner = "root";
      group = "root";
      setuid = true;
      source = "${pkgs.gllock}/bin/gllock";
    };
    cryptsetup = {
      owner = "root";
      group = "root";
      source = "${pkgs.cryptsetup}/bin/cryptsetup";
      capabilities = "cap_sys_admin+ep";
    };
  };
}
