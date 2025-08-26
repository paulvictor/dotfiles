{ config, pkgs, lib, specialArgs, ... }:

{
  imports = [ ./config/zsh.nix ];

  xdg.configFile."zsh/themes/spaceship.zsh-theme".source =
    "${pkgs.spaceship-prompt}/lib/spaceship-prompt/spaceship.zsh";
}
