# Command line tools and the editor that any host wants
{
  flake.modules.nixos.basics = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      file
      psmisc
      wget
    ];

    programs.git.enable = true;
    programs.htop.enable = true;

    programs.neovim = {
      enable = true;
      vimAlias = true;
      viAlias = true;
    };
  };
}
