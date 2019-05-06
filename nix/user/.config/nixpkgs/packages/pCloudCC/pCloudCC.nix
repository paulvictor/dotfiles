{stdenv, fetchFromGitHub , cmake, zlib, fuse, libudev, boost167, sqlite, openssl}:

with stdenv.lib;

stdenv.mkDerivation {
  name = "pCloudCC";
  sourceRoot = "source/pCloudCC";
  nativeBuildInputs = [ cmake ];
  buildInputs = [ zlib fuse libudev boost167 sqlite openssl ];
  src = fetchFromGitHub {
    owner = "plague-doctor";
    repo = "console-client";
    rev = "4c24bfffea60dc2b60a6ae876022ecbb08847971";
    sha256 = "0b5y4mhlqivs1jvglz3bqnfks2pm38mc8jy22fvcqa16d90d56gj";
  };
  patches = [ ./use-static_boost_libs_off.patch ];
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
