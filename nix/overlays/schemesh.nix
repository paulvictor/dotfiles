final: prev:

with final;
{
  schemesh = stdenv.mkDerivation {
    name = "schemesh";
    nativeBuildInputs = [ chez makeWrapper ];
    buildInputs = [ zlib ncurses libuuid ];
    src = final.fetchFromGitHub {
      owner = "cosmos72";
      repo = "schemesh";
      rev = "2a454dfcd40d9dd576dae91ebc32b528b98170ed";
      hash = "sha256-Y8jEGn4hkM3Xk0MFcFEil9+AIn3EHZBGiEUywf7oyPI=";
    };
    buildPhase = ''
     mkdir -pv $out/bin
     cp -a $src/* .

     DESTDIR=$out prefix= SCHEMESH_LIBDIR=/bin make -e install
     wrapProgram $out/bin/schemesh --add-flags "--library-dir $out/bin"
  '';
    phases = [ "buildPhase" ];
    meta = {
      mainProgram = "schemesh";
      description = "A Unix shell and Lisp REPL, fused together";
      homepage = "https://github.com/cosmos72/schemesh";
      license = lib.licenses.gpl2;
    };
  };
}
