{pkgs }:

with pkgs;
writeText "conf" ''
SURFRAW_text_browser=${lynx}/bin/lynx
SURFRAW_graphical_browser=${surf-webkit2}/bin/surf
SURFRAW_graphical_remote=no
SURFRAW_new_window=yes
SURFRAW_quote_args=no
SURFRAW_quote_ifs=no
SURFRAW_escape_url_args=yes
SURFRAW_graphical=yes
''
