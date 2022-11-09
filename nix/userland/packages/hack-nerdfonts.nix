{ stdenv, fetchzip, lib }:

stdenv.mkDerivation rec {
  name = "hack-nerdfonts-${version}";
  version = "2.2.2";

  src = fetchzip {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/v${version}/Hack.zip";
    sha256 = "sha256-Vz/v3LDGuvdXq+C3hkT1bDWJOSNwGZUKxudJXrtIsnE=";
    stripRoot = false;
  };
  buildCommand = ''
    install --target $out/share/fonts/opentype -D $src/*Mono.ttf
  '';

  meta = with lib; {
    description = "Nerdfont version of Hack";
    homepage = https://github.com/ryanoasis/nerd-fonts;
    license = licenses.mit;
  };
}
