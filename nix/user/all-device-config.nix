let
  makeDevice = devicePath:
    { config, pkgs, ... }:
    (import ./common-config.nix { inherit config pkgs; }) // (import devicePath { inherit config pkgs; });

#       {
#         imports = [
#           ./common-config.nix
#           devicePath
#         ];
#       };
  recCallPkgs = dir:
    let content = builtins.readDir dir; in
      builtins.listToAttrs
        (map (n: {name = n; value = makeDevice (dir + ("/" + n)); })
         (builtins.filter (n: builtins.pathExists (dir + ("/" + n + "/default.nix")))
           (builtins.attrNames content)));
in
  recCallPkgs ./devices
