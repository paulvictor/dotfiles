{pkgs ? (
  import <nixpkgs> {})}:

with pkgs;

let
  customizedEmacs = pkgs.callPackage ./packages/emax {};
in

writeShellScript "exwm-init.nix"
  ''
    ${customizedEmacs}/bin/emacs --init-directory=~/exwm/.emacs.d

  ''
