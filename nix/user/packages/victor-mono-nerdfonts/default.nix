{ stdenv, fetchzip, lib }:

stdenv.mkDerivation rec {
  name = "victor-mono-nerdfonts-${version}";
  version = "2.1.0";

  src = fetchzip {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/v${version}/VictorMono.zip";
    sha256 = "0scjbwzxrmawbmrnh1ggsr8zxz5fp47g0mrdb0hv7hqb80zlzw1s";
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

