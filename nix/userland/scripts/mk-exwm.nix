{pkgs ? (
  import <nixpkgs> {})}:

with pkgs;

let
  customizedEmacs = pkgs.callPackage ../../flake-parts/emax/package.nix {};
in

writeShellScript "exwm-init.nix"
  ''
    ${customizedEmacs}/bin/emacs --init-directory=~/exwm/.emacs.d

  ''
