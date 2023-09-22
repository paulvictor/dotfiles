{ config, pkgs, lib, specialArgs, ... }:

{
  programs.zsh = import ./config/zsh.nix { inherit pkgs config; };
  xdg.configFile."zsh/themes/spaceship.zsh-theme".source =
    "${pkgs.spaceship-prompt}/lib/spaceship-prompt/spaceship.zsh";
}
