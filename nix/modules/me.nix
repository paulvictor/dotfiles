{ lib, ... }:
let
  userSubmodule = lib.types.submodule {
    options = {
      username = lib.mkOption {
        type = lib.types.str;
        description = ''
          Login name of the primary user
        '';
      };
      fullname = lib.mkOption {
        type = lib.types.str;
        description = ''
          Full name, e.g. for git commits
        '';
      };
      email = lib.mkOption {
        type = lib.types.str;
        description = ''
          Primary email address
        '';
      };
      githubUsername = lib.mkOption {
        type = lib.types.str;
        description = ''
          GitHub username, e.g. for fetching authorized SSH keys
        '';
      };
      sshKey = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = ''
          File with SSH public key(s), e.g. fetched from github.com/<user>.keys
        '';
      };
      gpgKey = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          GPG key id / fingerprint
        '';
      };
      hashedPassword = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Hashed login password (mkpasswd output), for root and the user
        '';
      };
    };
  };
in
{
  imports = [
    ../me.nix
  ];
  options = {
    me = lib.mkOption {
      type = userSubmodule;
    };
  };
}
