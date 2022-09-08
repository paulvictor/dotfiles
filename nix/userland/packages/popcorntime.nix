{ lib
, stdenv
, nodejs
, mkYarnPackage
, nwjs
, makeWrapper
, fetchurl
, fetchYarnDeps
, fetchFromGitHub }:
let

  # We are using the Nwjs version referenced by popcorntime (0.44.5). Newer
  # versions seem to fail.
  # See:https://github.com/popcorn-official/popcorn-desktop/issues/2062
  expectedNwjsVersion = "0.44.5";

  nwjs' = nwjs.overrideAttrs (oldAttrs: rec {
    version = expectedNwjsVersion;
    src = fetchurl {
      url = "https://dl.nwjs.io/v${version}/nwjs-v${version}-linux-x64.tar.gz";
      sha256 = "sha256-LUs6wfQkpW8zH6WQOTcxSVOFGbQGtNg9EuiYGCsI3SQ=";
    };
  });

in
mkYarnPackage rec {
  pname = "popcorntime";
  version = "0.4.8";

  src = fetchFromGitHub {
    owner = "popcorn-official";
    repo = "popcorn-desktop";
    rev = "v${version}";
    sha256 = "sha256-/mxfYGHvWM6VnZpH3T5YSkrtC07XW5MAETxwhBIlOb0=";
  };

  nativeBuildInputs = [ makeWrapper ];

  postPatch = ''
    # Disable check for updates
    sed -i '178,179d' gulpfile.js
  '';

  postConfigure = ''
    rm deps/Popcorn-Time/node_modules
    cp -R "$node_modules" deps/Popcorn-Time
    chmod -R u+w deps/Popcorn-Time
  '';

  buildPhase = ''
    runHook preBuild
    cd deps/Popcorn-Time
    cache=cache/${expectedNwjsVersion}-sdk/linux64/
    mkdir -p $cache
    cp --no-preserve=mode -r ${nwjs'}/share/nwjs/. $cache
    npm run build
    runHook postBuild
  '';

  dontInstall = true;

  distPhase = ''
    runHook preDist
    mkdir -p $out/bin
    chmod +x build/Popcorn-Time/linux64/Popcorn-Time
    cp -r build/Popcorn-Time/linux64/. $out/
    makeWrapper $out/Popcorn-Time $out/bin/${pname}
    runHook postDist
  '';

  dontStrip = true;

  meta = with lib; {
    homepage = "https://github.com/popcorn-official/popcorn-desktop";
    description = "BitTorrent client that includes an integrated media player";
    maintainers = with maintainers; [ onny ];
    platforms = [ "x86_64-linux" ];
    license = licenses.gpl3Only;
  };
}
