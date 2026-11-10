# Command line tools and the editor that any host wants
{
  flake.modules.nixos.basics = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      file
      git
      htop
      psmisc
      wget
    ];

    programs.neovim = {
      enable = true;
      vimAlias = true;
      viAlias = true;
    };
  };
}
