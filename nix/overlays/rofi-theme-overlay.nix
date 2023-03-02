final: prev:

let
  src = final.fetchFromGitHub {
    owner = "davatorium";
    repo = "rofi-themes";
    rev = "bfdde8e7912ad50a468c721b29b448c1ec5fa5e3";
    sha256 = "w/AE1o8vIZdD0jAi7++gdlmApGjeyDv6CD4xxrD9Fsw=";
  };
in {
  rofi-themes = final.stdenv.mkDerivation {
    name = "rofi-themes";
    inherit src;
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/

      cp -Rv $src/User\ Themes/*rasi $out
      cp -Rv $src/Official\ Themes/*rasi $out

    '';
  };
}
