let
  makeDevice = devicePath:
    { config, pkgs, lib, ... }@args:
        import ./common-config.nix args;
  recCallPkgs = dir:
    let content = builtins.readDir dir; in
      builtins.listToAttrs
        (map (n: {name = n; value = makeDevice (dir + ("/" + n)); })
         (builtins.filter (n: builtins.pathExists (dir + ("/" + n + "/default.nix")))
           (builtins.attrNames content)));
in
  recCallPkgs ./devices
