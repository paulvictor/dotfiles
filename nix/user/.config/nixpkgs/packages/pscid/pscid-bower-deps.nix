{ pkgs , src }:

pkgs.stdenv.mkDerivation {
  name = "pscid-bower-deps";
  inherit src;
  phases = [ "unpackPhase" "buildPhase" ];
  bowerComponents = pkgs.buildBowerComponents {
    name = "pscid";
    generated = ./bower-packages.nix;
    inherit src;
  };
  buildPhase = ''
    #ln -sv $bowerComponents/bower_components .
    mkdir $out
    ln -sv $bowerComponents/bower_components $out
  '';
}
