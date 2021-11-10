{ config, pkgs, ...}:

{

  imports = [
    ../../services/mpd.nix
    ../../services/gocryptfs.nix
  ];
}
