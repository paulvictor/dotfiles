self: super:
let electronAppFromTemplate =
  {index, preload}:
    {url, appName}:
      super.writeShellScriptBin appName ''
        ${self.electron}/bin/electron ${index} ${url} ${appName} ${preload}
      '';
  allApps = map (electronAppFromTemplate {index = ./index.js; preload = ./preload.js;}) (builtins.fromJSON (super.lib.readFile ./apps.json));
in
  {
    electronApps = super.symlinkJoin {name = "electron-apps"; paths = allApps;};
  }
