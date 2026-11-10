# The primary user's identity (`config.me`), shared by NixOS and home-manager. On NixOS it also creates
# that user's account; root, sudo and the system-wide user policy are in features/users.
{
  flake.modules.nixos.me = { config, lib, pkgs, ... }: {
    imports = [ ./_me-options.nix ];

    users.users.${config.me.username} = {
      createHome = true;
      isNormalUser = true;
      inherit (config.me) hashedPassword;
      uid = 1000;
      # Groups a feature needs are added by that feature; `wheel` makes the user an administrator (sudo,
      # see features/users)
      extraGroups = [ "wheel" ];
      shell = pkgs.zsh;
      openssh.authorizedKeys.keyFiles = lib.optional (config.me.sshKey != null) config.me.sshKey;
    };

    nix.settings.trusted-users = [ config.me.username ];

    system.build.mkPrimaryUserHomeCryptPath =
      pkgs.runCommandLocal "mkPrimaryUserHomeCryptPath" {} ''
        mkdir -pv $out/persist/home/${config.me.username}/
        chown -R ${toString config.users.users.${config.me.username}.uid} $out/persist/home/${config.me.username}
      '';
  };

  flake.modules.homeManager.me = ./_me-options.nix;
}
