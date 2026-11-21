# Command line tools and the editor (vim) that any host wants
{
  flake.modules.nixos.basics = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      file
      psmisc
      wget
    ];

    programs.git.enable = true;
    programs.htop.enable = true;

    programs.vim = {
      enable = true;
      defaultEditor = true;
    };
  };
}
