# The sway session. NixOS: greetd login, portals, the sway binary and PAM. Home-manager: sway itself,
# waybar, mako, kanshi, swaylock/idle, launchers and wallpapers.
{
  flake.overlays.wallpapers = import ./_wallpaper-overlay.nix;

  flake.modules.nixos.sway = ./_nixos.nix;

  flake.modules.homeManager.sway = { lib, pkgs, ... }: {
    imports = [
      ./_wayland.nix
      ./_sway.nix
      ./_mako.nix
      ./_waybar.nix
      ./_guile-swayer.nix
    ];

    options.custom.sway.displayOffOnIdle = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Switch the displays off after being idle for a while";
    };

    config.home.packages = [
      pkgs.nerd-fonts.victor-mono # mako
      pkgs.font-awesome # waybar icons
      pkgs.roboto-mono # waybar text
    ];

    config.home.sessionVariables = {
      NIXOS_OZONE_WL = "1"; # For Chromium/Electron apps
      MOZ_ENABLE_WAYLAND = "1"; # For Firefox
      SDL_VIDEODRIVER = "wayland"; # For Games/SDL apps
      _JAVA_AWT_WM_NONREPARENTING = "1"; # For Java apps
    };
  };
}
