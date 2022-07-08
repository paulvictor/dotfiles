self: super:

let
  inherit (self) lib stdenv clangStdenv fuse zlib cmake boost autoPatchelfHook makeWrapper udev pkgconfig unzip;
  boostPkg = boost.override { enableShared = false; enabledStatic = true; };
  package = stdenv.mkDerivation rec {
    pname = "pcloudcc";
    version = "2.0.1";

    src = self.fetchFromGitHub {
      owner = "pcloudcom";
      repo = "console-client";
      rev = "4b42e3c8a90696ca9ba0a7e162fcbcd62ad2e306";
      sha256 = "9ccpIuWq8nV34GQac+b8CtGHTtSOAxpI/2h1i4AI97M=";
    };

    dontUseCmakeConfigure = true;

    nativeBuildInputs = [
      cmake
      boostPkg
      pkgconfig
      autoPatchelfHook
      makeWrapper
    ];

    buildInputs = [
      stdenv.cc.cc.lib
      pkgconfig
      fuse
      cmake
      zlib
      udev
      boostPkg
      unzip
    ];

    installPhase = ''
      mkdir -p $out/{bin,share}
      cp -r ./pCloudCC/* $out/share

      # Build pclsync
      cd $out/share/lib/pclsync
      make clean
      make fs

      # Build mbedtls
      cd $out/share/lib/mbedtls
      cmake . -Wno-dev
      make clean
      make

      # Build Application
      cd $out/share
      cmake . -Wno-dev
      make clean
      make
      make DESTDIR=$out CMAKE_INSTALL_PREFIX=$out install

      # Move the compiled binaries and library
      mv $out/var/empty/local/lib $out/
      mv $out/var/empty/local/bin $out/
    '';
  };
in { pcloud-console-client = package; }

