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
    xkb.layout = "us";
    exportConfiguration = true;
#     xkbVariant = ",common";
    xkb.model = "pc104";
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
