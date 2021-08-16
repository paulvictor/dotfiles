{ config, pkgs, lib, ...}:

let
  gllock = pkgs.callPackage ../common/pkgs/gllock.nix {};
  tomb-overlay = import ../common/pkgs/tomb.nix;
  firejail-overlay = import ../common/pkgs/firejail.nix;
  hm-overlay = import "${fetchTarball "https://github.com/nix-community/home-manager/tarball/master"}/overlay.nix";
  #hm = pkgs.callPackage (fetchTarball "https://github.com/rycee/home-manager/tarball/release-20.09") {};
  xkeyboardconfig-overlay =
    self: super:
      { xkeyboard_config =
          super.xkeyboard_config.overrideDerivation(oldAttrs: {
            src =
              fetchTarball
                https://www.x.org/releases/individual/data/xkeyboard-config/xkeyboard-config-2.28.tar.gz; }); };
in
{
  #nix.package = pkgs.nixUnstable;
  nix.nixPath = [
    "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
    #"nixpkgs-unstable=${fetchTarball "https://github.com/NixOS/nixpkgs/tarball/master"}"
    #"nixos-config=/etc/nixos/configuration.nix"
    "/nix/var/nix/profiles/per-user/root/channels"
    #"agenix=${fetchTarball "https://github.com/ryantm/agenix/archive/master.tar.gz"}"
    "home-manager=${fetchTarball "https://github.com/nix-community/home-manager/tarball/master"}" ];
  nixpkgs.overlays = [ hm-overlay tomb-overlay firejail-overlay ];
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [ "steghide-0.5.1" ];

  # Use the systemd-boot EFI boot loader.
#   boot.loader.systemd-boot.enable = true;
  networking.useDHCP = false;
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.
  networking.enableIPv6 = false;
  networking.extraHosts = let
    hostsPath = https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts;
    hostsFile = builtins.fetchurl hostsPath;
  in builtins.readFile "${hostsFile}";
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
  networking.networkmanager.insertNameservers = [ "8.8.8.8" "8.8.4.4" ];
  networking.firewall.enable = false;

  # Set your time zone.
  time.timeZone = "Asia/Kolkata";

  environment.systemPackages = with pkgs; [
    bc
    binutils
    bluez
    exfat
    exfat-utils
    file
    git
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
    xterm # Needed for some terminal settings for termite
    zsh
  ];

  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  systemd.services.pulseaudio.enable=false;
  systemd.sockets.pulseaudio.enable=false;
  hardware.pulseaudio = {
    enable = true;
    tcp = {
      enable = true;
      anonymousClients.allowAll = true;
    };
    package = pkgs.pulseaudioFull;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    daemon.logLevel = "debug";
    daemon.config = {
      flat-volumes = "no";
      enable-lfe-remixing = "no";
      allow-module-loading = "yes";
      log-target = "journal";
    };
  };
  services.journald.rateLimitInterval = "0";
  services.journald.rateLimitBurst = 0;
  services.journald.extraConfig = ''
    Storage=persistent
    MaxRetentionSec=3600
    SyncIntervalSec=10
    LineMax=100K
  '';
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  hardware.bluetooth.settings = {
    General = {
      Enable= "Source,Sink,Media,Socket";
    };
  };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  # Enable touchpad support.

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.mutableUsers = false;
  users.users.root = {
    shell = pkgs.zsh;
    hashedPassword = "$6$SCMbhhof$227ZIsJWgaZmuZX3gwWUTv4E5VrPaVKmZ/97cbU6yclJdn7To3F0ngRAcvmYX5mPOunW8bU6v16vqvxkqjivK.";
  };
  users.users.viktor = {
    createHome = true;
    isNormalUser = true;
    hashedPassword = "$6$SCMbhhof$227ZIsJWgaZmuZX3gwWUTv4E5VrPaVKmZ/97cbU6yclJdn7To3F0ngRAcvmYX5mPOunW8bU6v16vqvxkqjivK.";
    uid = 1000;
    extraGroups = [ "networkmanager" "audio" "wheel" "tty" "lp" "fuse" "docker" "adbusers" "netdev" "lxd" "disk" ];
    shell = pkgs.zsh;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.09"; # Did you read the comment?

  networking.networkmanager.enable = true;
  system.autoUpgrade.enable = false;
  system.autoUpgrade.channel = https://nixos.org/channels/nixos-unstable;
  services.xserver = {
    enable = true;
    libinput.enable = true;
    displayManager = {
      defaultSession = "xsession";
      autoLogin = {
        # This is because the HOME partition is anyway zfs encrypted and so would need a passphrase to mount
        enable = true;
        user = "viktor";
      };
      lightdm.enable = true;
      session = [
        {
          manage = "desktop";
          name = "xsession";
          start = ''exec $HOME/.xsession'';
        }
      ];
    };
    layout = "us,apl";
    xkbOptions = "grp:win_space_toggle,terminate:ctrl_alt_bksp";
    exportConfiguration = true;
    xkbVariant = ",common";
    xkbModel = "pc104";
    videoDrivers = [ "intel" "vesa" "modesetting" ];
    updateDbusEnvironment = true;
    enableCtrlAltBackspace = true;
#    extraLayouts.apl = {
#      description = "Custom APL config";
#      symbolsFile = "${fetchTarball https://www.x.org/releases/individual/data/xkeyboard-config/xkeyboard-config-2.28.tar.gz}/symbols/apl";
#      languages = ["eng"];
#    };
  };
  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = true;
  security.sudo.extraConfig = ''
    viktor ALL=(ALL) NOPASSWD: ALL
    #viktor ALL=(ALL) NOPASSWD: ${pkgs.tomb}/bin/tomb*, ${pkgs.systemd}/bin/systemctl*
  '';

  hardware.opengl = {
    enable = true;
    driSupport = true;
    # Only for intel
    extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
    ];
  };
  services.upower.enable = true;
  #services.nixosManual.showManual = true;
  programs.zsh.enable = true;
  programs.chromium = {
    enable = true;
    extensions = [
      "ihlenndgcmojhcghmfjfneahoeklbjjh"
      "mhpeahbikehnfkfnmopaigggliclhmnc"
    ];
  };
  services.dbus.packages = [ pkgs.bluez ];

  environment.etc."fuse.conf" = {
    text = ''
      user_allow_other
    '';
  };

  environment.etc."NetworkManager/system-connections" = {
    source = "/persist/etc/NetworkManager/system-connections/";
  };
  systemd.tmpfiles.rules = [
    "L /var/lib/bluetooth - - - - /persist/var/lib/bluetooth"
  ];

  virtualisation.docker= {
    enable = true;
    logDriver = "journald";
  };

  services.udev.packages = [ pkgs.crda pkgs.android-udev-rules pkgs.yubikey-personalization ];
  services.pcscd.enable = true;
  nix.trustedUsers = [ "@wheel" "viktor" "root" ];
  programs.adb.enable = true;
  nix.binaryCaches = [ "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
  nix.autoOptimiseStore = true;
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    #experimental-features = nix-command flakes
  '';
  nix.systemFeatures = [ "kvm" "big-parallel" ];
  services.zfs.autoScrub.enable = true;
  boot.loader.grub.copyKernels = true;
  boot.initrd.postDeviceCommands = pkgs.lib.mkAfter ''
    zfs rollback -r master/local/root@blank
  '';

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
    gllock.source = "${gllock}/bin/gllock";
    cryptsetup = {
      source = "${pkgs.cryptsetup}/bin/cryptsetup";
      capabilities = "cap_sys_admin+ep";
    };
  };
}
