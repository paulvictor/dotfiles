self: super:
  let
    appDefs = import ./apps.nix;
    electronCmd = {url, name, proxy ? null, ...}:
      let
        proxyFlag =
          super.lib.optionalString
            (proxy != null)
            "--proxy-server=${proxy} ";
      in "${self.electron}/bin/electron ${proxyFlag} ${./index.js} ${url} ${name}";
    allApps = map
      ({name, ...} @ app:
        super.writeShellScriptBin name ''${electronCmd app}'')
      appDefs;
    allDesktopEntries = map
      ({url, name, ...} @ app:
        super.makeDesktopItem {
          name = name;
          desktopName = name;
          exec = electronCmd app;
          type = "Application";
          categories = [ "Network" ];
        })
      appDefs;
  in
  {
    electronApps = super.symlinkJoin {name = "electron-apps"; paths = allApps ++ allDesktopEntries;};
  }
