args@{ config, lib, pkgs, ... } :

let
  mkHostKeyDirPath = {config, pkgs, ...}:
    let
      hostKeyPath = "/tomb/${config.networking.hostName}/ssh/ssh_host_rsa_key";
    in
    {
      system.build.mkHostKeyDirPath = pkgs.runCommandLocal "mkHostKeyPath" {} ''
        mkdir -pv $out/$(dirname ${hostKeyPath})
      '';
    };
  sshConfig = { config, lib, pkgs, ... }:
    let
      hostKeyPath = "/tomb/${config.networking.hostName}/ssh/ssh_host_rsa_key";
    in
    {
      services.openssh = {
        enable = true;
        settings = {
#           ForwardX11 = true;
          PermitRootLogin = lib.mkDefault "yes";
          GatewayPorts = "yes";
          PasswordAuthentication = false;
        };
#         authorizedKeysCommand =
#           with pkgs;
#           let
#             script =
#               writeShellScript
#                 "getMyKey"
#                 "${curl}/bin/curl -L https://github.com/paulvictor.keys";
#           in lib.mkDefault (toString script); # Google cloud images may want to override this
        hostKeys =
          [
            {
              path = hostKeyPath;
              type = "rsa";
              bits = 4096;
            }
          ];
      };
    };
in
{
  imports = [ mkHostKeyDirPath sshConfig ];
}
