{ config, lib, pkgs, ... } :
{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.mutableUsers = false;
  users.users.root = {
    shell = pkgs.zsh;
    inherit (config.me) hashedPassword;
  };
  users.users.${config.me.username} = {
    createHome = true;
    isNormalUser = true;
    inherit (config.me) hashedPassword;
    uid = 1000;
    extraGroups = [ "input" "networkmanager" "audio" "wheel" "tty" "lp" "fuse" "docker" "adbusers" "netdev" "lxd" "disk" "video" "keys" "libvirtd" "qemu-libvirtd" "pipewire" "ydotool" "ykusers" "kvm" ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keyFiles = lib.optional (config.me.sshKey != null) config.me.sshKey;
  };

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;
  security.sudo.extraConfig = ''
    ${config.me.username} ALL=(ALL) NOPASSWD: ALL
  '';

  nix.settings.trusted-users = [ "@wheel" config.me.username ];

  environment.etc."fuse.conf" = {
    text = ''
      user_allow_other
    '';
  };

  system.build.mkPrimaryUserHomeCryptPath = pkgs.runCommandLocal "mkPrimaryUserHomeCryptPath" {} ''
    mkdir -pv $out/persist/home/${config.me.username}/
    chown -R ${toString config.users.users.${config.me.username}.uid} $out/persist/home/${config.me.username}
  '';
}
