{ config, pkgs, lib, specialArgs, system, ... }:

# # TODO : Try to refactor with https://discourse.nixos.org/t/using-mkif-with-nested-if/5221/2
# # Also https://discourse.nixos.org/t/infinite-recursion-on-optional-import/8892/3
{
  news.display = "silent";
  manual = {
    html.enable = false;
    json.enable = false;
    manpages.enable = false;
  };
  imports =
    [
      ./modules/services/gocryptfs.nix
      ./modules/services/battery-alert.nix
      ./common.nix
    ]
    ++ (lib.optional specialArgs.withGUI ./gui-config.nix)
    ++ (lib.optional specialArgs.isDevEnv ./dev-config.nix)
    ++ (lib.optional specialArgs.isDesktop ./desktop-config.nix)
    ++ specialArgs.hostSpecificImports;
}
