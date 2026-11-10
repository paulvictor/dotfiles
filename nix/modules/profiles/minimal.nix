# The least a server or cloud instance needs: nix, the primary user, ssh.
{
  profiles.minimal = {
    nixos = [ "nix" "me" "user" "ssh" ];
    home = [ "me" "home-base" ];
  };
}
