{ config, lib, pkgs, ... } :

{
  imports = [
    ./key-remaps.nix
    ./pipewire.nix
  ];
  security.polkit.enable = true;
  hardware.opengl.enable = true; # when using QEMU KVM
  services.greetd = {
    enable = true;
    settings = {
     default_session.command = ''
      ${pkgs.greetd.tuigreet}/bin/tuigreet \
        --time \
        --user-menu \
        --cmd sway
    '';
    };
  };

  environment.etc."greetd/environments".text = ''
    sway
  '';
  services.xserver = {
    enable = false;
    libinput.enable = true;
    displayManager = {
      defaultSession = "xsession";
      autoLogin = {
        # This is because the safe partition is anyway zfs encrypted and so would need a passphrase to mount
        enable = true;
        user = "viktor";
      };
#       lightdm.enable = true;
      # disabled lightdm because it causes some problems for nyxt
      session = [
        {
          manage = "desktop";
          name = "xsession";
          start = ''exec $HOME/.xsession'';
        }
      ];
    };
    layout = "us";
    exportConfiguration = true;
#     xkbVariant = ",common";
    xkbModel = "pc104";
    updateDbusEnvironment = true;
#     enableCtrlAltBackspace = true;
    #    extraLayouts.apl = {
    #      description = "Custom APL config";
    #      symbolsFile = "${fetchTarball https://www.x.org/releases/individual/data/xkeyboard-config/xkeyboard-config-2.28.tar.gz}/symbols/apl";
    #      languages = ["eng"];
    #    };
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

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    disabledPlugins = [ "sap" ];
#     hsphfpd.enable = true;
    package = pkgs.bluez;
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
