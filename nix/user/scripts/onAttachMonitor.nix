{ pkgs, config }:

with pkgs;
writeText "onAttachMonitor" ''
#   sleep 3 && ${xorg.xmodmap}/bin/xmodmap -verbose ${config.home.homeDirectory}/.Xmodmap
  ${feh}/bin/feh --bg-scale ${wall1} ${wall2} ${wall3}
''
