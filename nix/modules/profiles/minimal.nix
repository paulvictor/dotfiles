# The least a server or cloud instance needs: nix, the primary user (me), the system user policy (users), ssh,
# and the basic command line tools.
{ config, ... }: {
  profiles.minimal = {
    nixos = with config.features.nixos; [ nix me users ssh basics ];
    home = with config.features.home; [ me home-base ];
  };
}
