{pkgs, config, lib, ...}:
let
  inherit (pkgs) warpd;
in
{
  home.packages = [ warpd; ];

  xdg.configFile."warpd/config" = {
    enable = true;
    text = "";
  };

}
