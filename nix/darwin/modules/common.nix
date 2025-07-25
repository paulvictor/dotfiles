{config, pkgs, lib, ...}:

let
  system = pkgs.stdenv.system;
in
{
  system.stateVersion = 6;
  nix.package = pkgs.nixStable;
  nix.extraOptions = ''
      experimental-features = nix-command flakes
      gc-keep-outputs = true
      gc-keep-derivations = true
      build-users-group = nixbld
    '';
  environment.systemPackages = with pkgs;[
    gitFull
    coreutils
    htop
    darwin-rebuild darwin-option
  ];

  environment.shells = [ pkgs.bashInteractive pkgs.zsh ];
  environment.systemPath = [ "/run/current-system/sw/bin" ];

  programs.bash.enable = true;
  documentation.enable = true;
  documentation.man.enable = true;
  documentation.doc.enable = true;
  documentation.info.enable = true;
}
