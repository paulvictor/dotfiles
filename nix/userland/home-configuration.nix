{ config, pkgs, lib, specialArgs, ... }:

# # TODO : Try to refactor with https://discourse.nixos.org/t/using-mkif-with-nested-if/5221/2
# # Also https://discourse.nixos.org/t/infinite-recursion-on-optional-import/8892/3
with pkgs;
{
  news.display = "silent";
  manual = {
    html.enable = true;
    json.enable = true;
  };
  imports =
    [
      ./modules/services/gocryptfs.nix
      ./common.nix
      ./dev-config.nix
      ./desktop-config.nix
      ./gui-config.nix
    ];
}
