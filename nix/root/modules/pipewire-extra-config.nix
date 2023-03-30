{ config, pkgs, ... }:

with pkgs;
{
  environment.etc =
    builtins.listToAttrs
      (builtins.map
        (file:
          lib.attrsets.nameValuePair
            "pipewire/${file}"
            ({ text = builtins.readFile (./pipewire-configs + ("/" + file)); }))
        (lib.attrNames (builtins.readDir ./pipewire-configs)));
}
