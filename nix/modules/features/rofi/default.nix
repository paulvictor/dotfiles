# rofi launcher with fuzzy matching and the rofi-themes collection
{
  flake.overlays.rofi-fuzzy = import ./_overlay.nix;

  flake.modules.homeManager.rofi = { pkgs, ... }: {
    home.packages = [
      pkgs.rofi
      pkgs.nerd-fonts.sauce-code-pro # the font in _config.nix
    ];
    xdg.configFile.rofi = {
      recursive = true;
      source = pkgs.fetchFromGitHub {
        owner = "davatorium";
        repo = "rofi-themes";
        rev = "bfdde8e7912ad50a468c721b29b448c1ec5fa5e3";
        sha256 = "w/AE1o8vIZdD0jAi7++gdlmApGjeyDv6CD4xxrD9Fsw=";
      };
    };
    xdg.configFile."rofi/config.rasi".source = import ./_config.nix { inherit pkgs; };
  };
}
