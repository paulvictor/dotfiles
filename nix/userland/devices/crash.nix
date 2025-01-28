{ lib, ... }:

{
  programs.ssh.enable = lib.mkForce false;
}
