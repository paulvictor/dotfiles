{ pkgs }:
with pkgs;
writeShellScriptBin "pursuit" ''
${qutebrowser}/bin/qutebrowser "https://pursuit.purescript.org/search?q=$1" & 2>/dev/null
${xdotool}/bin/xdotool search --sync --class qutebrowser >/dev/null # Wait for the browser app to load up.
${i3-gaps}/bin/i3-msg "[class=qutebrowser] focus" # Change focus in window manager
''
