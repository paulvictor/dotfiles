{ pkgs }:
with pkgs;
writeShellScriptBin "findWindowByTitle" ''
${i3-gaps}/bin/i3-msg -t GET_TREE | ${jq}/bin/jq -M "recurse(.nodes[]) | select(has(\"window_properties\")) | {name, id, type, window_properties} | select(.window_properties.title == \"$1\" and .window_properties.class == \"Termite\") | .id"
''
