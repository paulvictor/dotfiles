{ config, lib, pkgs, ... }:
let
  swayWithEnv = pkgs.writeShellScript "sway-with-env" ''
    export XDG_SESSION_TYPE=wayland
    export XDG_SESSION_DESKTOP=sway
    export XDG_CURRENT_DESKTOP=sway
    export NIXOS_OZONE_WL=1           # For Chromium/Electron apps
    export MOZ_ENABLE_WAYLAND=1
    export SDL_VIDEODRIVER=wayland    # For Games/SDL apps
    export _JAVA_AWT_WM_NONREPARENTING=1 # For Java apps
    exec ${pkgs.sway}/bin/sway ${lib.optionalString config.hardware.nvidia.enabled "--unsupported-gpu"} "$@"
  '';
in
{
  security.pam.services.swaylock.text = "auth include login";
  programs.ydotool.enable = true;

  services.greetd = {
    enable = true;
    settings = {
      default_session.command = ''${pkgs.tuigreet}/bin/tuigreet --time --user-menu --remember --cmd ${swayWithEnv}'';
      # Autologin on boot; after logout greetd falls back to default_session.
      # This is because the safe partition is anyway zfs encrypted and so would need a passphrase to mount
      initial_session = {
        command = "${swayWithEnv}";
        user = config.me.username;
      };
    };
  };
  environment.etc."greetd/environments".text = ''
    sway
  '';

  # TODO screen sharing doesnt work yet
  # see https://github.com/emersion/xdg-desktop-portal-wlr/wiki/%22It-doesn't-work%22-Troubleshooting-Checklist
  # https://mozilla.github.io/webrtc-landing/gum_test.html
  # https://soyuka.me/make-screen-sharing-wayland-sway-work/
  # https://github.com/emersion/xdg-desktop-portal-wlr/blob/master/contrib/wlroots-portals.conf
  # https://gitlab.freedesktop.org/pipewire/wireplumber/-/issues/454
  # https://github.com/emersion/xdg-desktop-portal-wlr/wiki/Screencast-Compatibility
  # https://discourse.nixos.org/t/xdg-portals-all-broken/48308/9
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
