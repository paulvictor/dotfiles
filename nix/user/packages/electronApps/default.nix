self: super:
let electronAppFromTemplate =
#   {index, preload}:
#         ${self.electron}/bin/electron ${index} ${url} ${appName} ${preload}
    {url, appName}:
      let
        erwicFile = super.writeText "${appName}-erwic"
          (builtins.toJSON
            { name = appName;
              apps = [
                { container = appName; inherit url; } ]; });
      in
        super.writeShellScriptBin
          appName
          "${self.vieb}/bin/vieb --datafolder=$HOME/plain/electron-apps/${appName} --erwic=${erwicFile}";
  allApps = map electronAppFromTemplate (builtins.fromJSON (super.lib.readFile ./apps.json));
in
  {
    electronApps = super.symlinkJoin {name = "electron-apps"; paths = allApps;};
  }
