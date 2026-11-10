{pkgs, rofi, electronApps}:

with pkgs;

writeShellScriptBin "rofiElectronAppsRunner" ''
  choice=$(ls ${electronApps}/bin | ${rofi}/bin/rofi -i -dmenu)
  [[ -n $choice ]] && exec ${electronApps}/bin/$choice
''
