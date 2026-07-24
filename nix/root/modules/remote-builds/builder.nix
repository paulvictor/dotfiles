{ config, pkgs, ... }:

{
  users.users.builder = {
    isSystemUser = true;
    group = "builder";
    shell = pkgs.bash;
    hashedPassword = "!";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM1EJE3rRE8HVOHGSreCV5WAOTBMErTiCJTAChsUAS4f"
    ];
  };
  users.groups.builder = {};

  nix.settings.trusted-users = [ "builder" ];

  services.openssh.settings.AllowUsers = [ "builder" ];
}
