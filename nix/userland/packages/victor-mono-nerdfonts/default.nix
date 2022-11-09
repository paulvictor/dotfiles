{ stdenv, fetchzip, lib }:

stdenv.mkDerivation rec {
  name = "victor-mono-nerdfonts-${version}";
  version = "2.2.2";

  src = fetchzip {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/v${version}/VictorMono.zip";
    sha256 = "sha256-TYDl421wyAOai/5Rjdj3IrxT9oo2kN4nlB7zv4MB1CQ=";
    stripRoot = false;
  };
  buildCommand = ''
    install --target $out/share/fonts/opentype -D $src/*.ttf
  '';

  meta = with lib; {
    description = "Nerdfont version of SourceCodePro Fonts";
    homepage = https://github.com/ryanoasis/nerd-fonts;
    license = licenses.mit;
  };
}

