{stdenv, fetchFromGitHub , cmake, zlib, fuse, libudev, boost167, sqlite, openssl, curl}:

with stdenv.lib;

stdenv.mkDerivation rec {
  name = "pCloudCC";
  sourceRoot = "source/pCloudCC";
  nativeBuildInputs = [ cmake ];
  buildInputs = [ zlib fuse libudev boost167 sqlite openssl curl ];
  #dontUseCmakeConfigure = false;
  installPhase = ''
    mkdir -pv $out/bin
    mkdir -pv $out/lib

    cp libpcloudcc_lib.so $out/lib
    cp pcloudcc $out/bin
    '';
  fixupPhase = ''
    patchelf --set-rpath $out/lib:${makeLibraryPath (buildInputs ++ [ stdenv.cc.cc.lib ])} $out/bin/pcloudcc
    '';
  src = fetchFromGitHub {
    owner = "plague-doctor";
    repo = "console-client";
    rev = "7df58bbdf047c4261d5b69103a6bdecf762d3170";
    sha256 = "01wqsgfih7aln74wy6w4bk1jbwrz5c4iy8kczgx8azdz1qrjfxmr";
  };
  patches = [ ./use-static_boost_libs_off.patch ./nixos-version.patch ];
  patchFlags = [ "-p2" ];
  preBuild = ''
    cd ../lib/pclsync/ && \
    make clean && \
    make fs && \
    cd ../mbedtls/ && \
    cmake . && \
    make clean && \
    make && \
    cd ../../build/
    '';
}
