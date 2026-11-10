# The minimum home-manager needs: who the user is, and quiet defaults
{
  flake.modules.homeManager.home-base = { config, lib, ... }: {
    home.username = lib.mkDefault config.me.username;
    home.homeDirectory = lib.mkDefault "/home/${config.me.username}";

    news.display = "silent";
    manual = {
      html.enable = false;
      json.enable = false;
      manpages.enable = false;
    };
  };
}
