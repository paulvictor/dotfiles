{ stdenv, fetchFromGitHub, lib }:

stdenv.mkDerivation rec {
  name = "all-the-icons-fonts";

  src = fetchFromGitHub {
    "owner" = "domtronn";
    "repo" =  "all-the-icons.el";
    "rev" = "c0d288a41faea2ecb7e8dd947486764a2ee17ec9";
    "sha256" = "1r1905irz9rh05qzmzk1cbdnk4667ax8wm71r1prv8dnx8nq05kp";
    "fetchSubmodules" = true;
  };
  buildCommand = ''
    install --target $out/share/fonts/truetype -D $src/fonts/*.ttf
  '';

}

