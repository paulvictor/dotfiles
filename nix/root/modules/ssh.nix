args@{ config, lib, pkgs, ... } :

let
  hostKeyPath = "/tomb/${config.networking.hostName}/ssh/ssh_host_rsa_key";
  mkHostKeyDirPath = {pkgs, ...}:
    {
      system.build.mkHostKeyDirPath = pkgs.runCommandLocal "mkHostKeyPath" {} ''
        mkdir -pv $out/$(dirname ${hostKeyPath})
      '';
    };
  sshConfig = { lib, ... }:
    {
      services.openssh = {
        enable = true;
        settings = {
          PermitRootLogin = lib.mkDefault "yes";
          GatewayPorts = "yes";
          PasswordAuthentication = false;
          StrictModes = false;
        };
      };
    };
in
{
  imports = [ sshConfig ];
}
