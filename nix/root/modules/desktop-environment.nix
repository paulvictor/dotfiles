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

  # TODO screen sharing doesnt work yet
  # see https://github.com/emersion/xdg-desktop-portal-wlr/wiki/%22It-doesn't-work%22-Troubleshooting-Checklist
  # https://mozilla.github.io/webrtc-landing/gum_test.html
  # https://soyuka.me/make-screen-sharing-wayland-sway-work/
  # https://github.com/emersion/xdg-desktop-portal-wlr/blob/master/contrib/wlroots-portals.conf
  # https://gitlab.freedesktop.org/pipewire/wireplumber/-/issues/454
  # https://github.com/emersion/xdg-desktop-portal-wlr/wiki/Screencast-Compatibility
  # https://discourse.nixos.org/t/xdg-portals-all-broken/48308/9
  # https://mynixos.com/nixpkgs/option/xdg.portal.config
  # https://github.com/emersion/xdg-desktop-portal-wlr/wiki/%22It-doesn't-work%22-Troubleshooting-Checklist
  # https://github.com/emersion/xdg-desktop-portal-wlr/blob/master/contrib/wlroots-portals.conf
  # https://github.com/NixOS/nixpkgs/blob/nixos-24.11/nixos/modules/config/xdg/portal.nix
  xdg.portal = {
    enable = true;

    wlr.enable = true;
    wlr.settings = {
      screencast = {
        output_name = "HDMI-A-1";
        max_fps = 30;
        chooser_type = "simple";
        chooser_cmd = "${pkgs.slurp}/bin/slurp -f %o -or";
      };
    };
    extraPortals = [
      pkgs.xdg-desktop-portal-wlr
      pkgs.xdg-desktop-portal-gtk
    ];
    configPackages = [pkgs.xdg-desktop-portal-wlr pkgs.xdg-desktop-portal-gtk];
    config = {
      common = {
        default = ["gtk"];
      };
      wlroots = {
        "org.freedesktop.impl.portal.Screenshot" = "wlr";
        "org.freedesktop.impl.portal.ScreenCast" = "wlr";
      };
    };

  };
  environment.systemPackages = with pkgs; [
    xdg-desktop-portal-wlr
    xdg-desktop-portal-gtk
  ];

  programs.sway = {
    enable = true;
    package = null;
  };

}
# [screencast]
# output_name=
# max_fps=30
# chooser_cmd=slurp -f %o -or
# chooser_type=simple
