{ config, pkgs, ...}@args:

{
  import ../../services/gocryptfs.nix args;

#   imports = [
# #     ../../services/mpd.nix
#     ../../services/gocryptfs.nix
#   ];
}
