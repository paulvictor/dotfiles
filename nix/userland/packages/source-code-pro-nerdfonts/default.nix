{ stdenv, fetchzip, lib }:

stdenv.mkDerivation rec {
  name = "source-code-pro-nerdfonts-${version}";
  version = "2.2.2";

  src = fetchzip {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/v${version}/SourceCodePro.zip";
    sha256 = "sha256-aFKUlC94lAYWTcXuKhqAvJ0kbNZoBtvPknOeRGEnpV0=";
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
