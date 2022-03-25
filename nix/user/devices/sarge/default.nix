{ config, pkgs, ...}@args:

{
  import ../../services/gocryptfs.nix args;
}
