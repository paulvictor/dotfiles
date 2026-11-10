# System-wide user policy: users are declared, not mutable; zsh for root and the primary user; root;
# sudo; who may use nix as a trusted user. The primary user's account itself is in modules/me.nix.
{
  flake.modules.nixos.users = { config, pkgs, ... }: {
    users.mutableUsers = false;
    programs.zsh.enable = true; # the login shell of root and the primary user

    users.users.root = {
      shell = pkgs.zsh;
      inherit (config.me) hashedPassword;
    };

    security.sudo.enable = true;
    security.sudo.wheelNeedsPassword = false;

    nix.settings.trusted-users = [ "@wheel" ];

    environment.etc."fuse.conf".text = ''
      user_allow_other
    '';
  };
}
