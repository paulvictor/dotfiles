{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  src = pkgs.fetchFromGitHub {
    owner = "kuravih";
    repo = "gllock";
    rev = "8123a6566e4e11f54b2ec1bd9469129fc44cd03b";
    sha256 = "145012lfdsrc58vdidy6vz4l324ihq35k83gxjbyv472xz6rx6nj";
    fetchSubmodules = true;
  };
  name = "gllock";
  buildInputs = with pkgs; [ xorg.libX11 xorg.xorgproto glew110 glxinfo ];
  patchPhase = ''
    echo "SHADER_LOCATION=${src}/shaders" >> config.mk
    sed -i 's/circle/ascii/' config.mk
  '';
  installPhase = ''
    mkdir -pv $out/bin
    cp gllock $out/bin
  '';
}
