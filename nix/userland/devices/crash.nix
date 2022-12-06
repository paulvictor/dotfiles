{ config, pkgs, lib, specialArgs, ... }:

{
  programs.ssh.enable = lib.mkForce false;
}
