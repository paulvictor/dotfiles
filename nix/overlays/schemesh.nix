final: prev:

with final;
{
  schemesh = stdenv.mkDerivation {
    name = "schemesh";
    nativeBuildInputs = [ chez makeWrapper ];
    buildInputs = [ zlib ncurses libuuid ];
    src = fetchFromGitHub {
      owner = "cosmos72";
      repo = "schemesh";
      rev = "bec853bd57db718c39c2ca53a03d373ff4675bd7";
      hash = "sha256-mrmdadHagUFT7rg4C+p6rjcoMM2Io4t3wQZDRQqe8fE=";
    };
    buildPhase = ''
     mkdir -pv $out/bin
     cp -a $src/* .

     DESTDIR=$out prefix= SCHEMESH_LIBDIR=/bin make -e install
     wrapProgram $out/bin/schemesh --add-flags "--library-dir $out/bin"
  '';
    phases = [ "buildPhase" ];
  };
}
