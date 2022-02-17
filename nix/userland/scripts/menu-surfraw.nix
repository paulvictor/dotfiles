{pkgs}:

with pkgs;
writeShellScriptBin "menu-surfraw" ''
  elvi=$(${surfraw}/bin/surfraw -elvi | \
    ${gawk}/bin/awk -F'-' '{print $1}' | \
    ${gnused}/bin/sed '/:/d' | \
    ${gawk}/bin/awk '{$1=$1};1' | \
    ${coreutils}/bin/sort | \
    ${coreutils}/bin/uniq | \
    ${rofi}/bin/rofi -dmenu -theme onedark -i -p "search: ")
  #[[ $elvi -eq "" ]] && exit 0;
  searchTerm=$(echo "" | ${rofi}/bin/rofi -dmenu -theme onedark -p "term: ")
  exec ${surfraw}/bin/surfraw $elvi $searchTerm
''
