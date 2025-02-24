{ stdenv, fetchFromGitHub, lib }:

stdenv.mkDerivation rec {
  name = "all-the-icons-fonts";

  src = fetchFromGitHub {
    "owner" = "domtronn";
    "repo" =  "all-the-icons.el";
    rev = "39ef44f810c34e8900978788467cc675870bcd19";
    hash = "sha256-SlqJ8erFtiydEdgZFGJ23DQ9fZH4FF09iO6t3gf5pwE=";
    "fetchSubmodules" = true;
  };
  buildCommand = ''
    install --target $out/share/fonts/truetype -D $src/fonts/*.ttf
  '';

}

