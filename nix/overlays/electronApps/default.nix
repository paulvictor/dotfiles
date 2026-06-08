self: super:
  let
    appDefs = import ./apps.nix;
    allApps = map
      ({url, name, proxy ? null}:
        super.stdenv.mkDerivation {
          inherit name;
          nativeBuildInputs = [ super.makeWrapper ];
          buildCommand = ''
            makeWrapper ${self.electron}/bin/electron $out/bin/${name} \
              --set ELECTRON_APP_URL "${url}" \
              --set ELECTRON_APP_NAME "${name}" \
              ${super.lib.optionalString (proxy != null) ''--set ELECTRON_APP_PROXY "${proxy}"''} \
              --add-flags "${./index.js}"
          '';
        })
      appDefs;
    allDesktopEntries = map
      ({name, ...}:
        super.makeDesktopItem {
          inherit name;
          desktopName = name;
          exec = name;
          type = "Application";
          categories = [ "Network" ];
        })
      appDefs;
  in
  {
    electronApps = super.symlinkJoin {name = "electron-apps"; paths = allApps ++ allDesktopEntries;};
  }
