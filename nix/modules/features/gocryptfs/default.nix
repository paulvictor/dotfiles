# gocryptfs mount service (currently disabled)
{
  flake.modules.homeManager.gocryptfs = { config, ... }: {
    imports = [ ./_service.nix ];
    services.gocryptfs = {
      enable = false;
      cryptDir = "${config.home.homeDirectory}/crypt";
      plainDir = "${config.home.homeDirectory}/plain";
      passCmd = "cat /run/secrets/crypt-mount-key";
    };
  };
}
