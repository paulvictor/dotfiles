{ pkgs, config }:

with pkgs;
let wallpaper = fetchurl {
  url = "https://wallpapercave.com/wp/NOFQh9F.png";
  sha256 = "0n9f4wb4z01ynv6zhi0r0m9hv3fax3vldbrhc8s90y9hwxn7nkn5";
};
in
writeText "onAttachMonitor" ''
  ${xorg.xmodmap}/bin/xmodmap ${config.home.homeDirectory}/.Xmodmap
  ${compton}/bin/compton -z -D 0 -m 0.7 --inactive-dim 0.2 --inactive-dim-fixed -b -C --focus-exclude "x = 0 && y = 0 && override_redirect = true"  --focus-exclude '_NET_WM_NAME@:s = "rofi"' --backend glx
  ${feh}/bin/feh --bg-scale ${wallpaper}
  ${pasystray}/bin/pasystray &
''
