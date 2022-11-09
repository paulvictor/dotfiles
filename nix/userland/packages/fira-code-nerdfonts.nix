{ stdenv, fetchzip, lib }:

stdenv.mkDerivation rec {
  name = "fira-code-nerdfonts-${version}";
  version = "2.2.2";

  src = fetchzip {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/v${version}/FiraCode.zip";
    sha256 = "sha256-rXRHi5B867H25I1I2bD2idjbdv9kcQbkv4j00npREiU=";
    stripRoot = false;
  };
  buildCommand = ''
    install --target $out/share/fonts/opentype -D $src/*Mono*.ttf
    rm -Rf $out/share/fonts/opentype/*Windows\ Compatible.ttf
  '';

  meta = with lib; {
    description = "Nerdfont version of Fira Code";
    homepage = https://github.com/ryanoasis/nerd-fonts;
    license = licenses.mit;
  };
}
