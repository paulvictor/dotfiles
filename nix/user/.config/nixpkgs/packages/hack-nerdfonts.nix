{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "hack-nerdfonts-${version}";
  version = "2.0.0";

  src = fetchzip {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/v${version}/Hack.zip";
    sha256 = "11qxjf1i55d7w869a9yps8k5gsjzr2g2cvr7zw08ivlzm0g9x3mk";
    stripRoot = false;
  };
  buildCommand = ''
    install --target $out/share/fonts/opentype -D $src/*Mono.ttf
  '';

  meta = with stdenv.lib; {
    description = "Nerdfont version of Hack";
    homepage = https://github.com/ryanoasis/nerd-fonts;
    license = licenses.mit;
  };
}
