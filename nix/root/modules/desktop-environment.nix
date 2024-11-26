{ config, lib, pkgs, ... } :
let
  swayWithEnv = pkgs.writeShellScript "sway-with-env" ''
    export XDG_SESSION_TYPE=wayland
    export XDG_SESSION_DESKTOP=sway
    export XDG_CURRENT_DESKTOP=sway

    export MOZ_ENABLE_WAYLAND=1
    exec ${pkgs.sway}/bin/sway "$@"
  '';

in {
  imports = [
    ./key-remaps.nix
    ./pipewire.nix
  ];
  services.libinput.enable = true;
#   security.polkit.enable = lib.mkForce false;
  hardware.graphics.enable = true; # when using QEMU KVM
  security.pam.services.swaylock.text = "auth include login";
  programs.ydotool.enable = true;

  services.greetd = {
    enable = true;
    settings = {
      default_session.command = ''
        ${pkgs.greetd.tuigreet}/bin/tuigreet \
          --time \
          --user-menu \
          --remember \
          --cmd ${swayWithEnv}
      '';
    };
  };
  environment.etc."greetd/environments".text = ''
    sway
  '';


#   services.displayManager.defaultSession = "xsession";
  services.displayManager.autoLogin = {
    # This is because the safe partition is anyway zfs encrypted and so would need a passphrase to mount
    enable = true;
    user = "viktor";
  };
  services.xserver = {
    enable = false;
    displayManager = {
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
