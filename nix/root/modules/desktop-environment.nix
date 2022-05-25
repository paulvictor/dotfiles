{ config, lib, pkgs, ... } :

{
  imports = [
    ./key-remaps.nix
    ./pipewire.nix
  ];
  services.xserver = {
    enable = true;
    libinput.enable = true;
    displayManager = {
      defaultSession = "xsession";
      autoLogin = {
        # This is because the safe partition is anyway zfs encrypted and so would need a passphrase to mount
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

  programs.chromium = {
    enable = true;
    extensions = [
      "ihlenndgcmojhcghmfjfneahoeklbjjh"
      "mhpeahbikehnfkfnmopaigggliclhmnc"
    ];
  };
  services.dbus.packages = [ pkgs.bluez ];

  programs.light.enable = true;

  # Enable sound.
#   sound.enable = true;
#   systemd.services.pulseaudio.enable=false;
#   systemd.sockets.pulseaudio.enable=false;
#   hardware.pulseaudio = {
#     enable = true;
#     tcp = {
#       enable = true;
#       anonymousClients.allowAll = true;
#     };
#     package = pkgs.pulseaudioFull;
# #     extraModules = [ pkgs.pulseaudio-modules-bt ];
#     daemon.logLevel = "debug";
#     daemon.config = {
#       flat-volumes = "no";
#       enable-lfe-remixing = "no";
#       allow-module-loading = "yes";
#       log-target = "journal";
#     };
#   };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    disabledPlugins = [ "sap" ];
    hsphfpd.enable = true;
    package = pkgs.bluezFull;
  };
  environment.etc."bluetooth/audio.conf".text =
    lib.generators.toINI {} {
      General = {
        Enable= "Source,Sink,Media,Socket";
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

  programs.dconf.enable = true;

}
