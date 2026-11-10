# An electron wrapper around a web page, with a desktop entry
{ lib, stdenv, makeWrapper, electron, makeDesktopItem, symlinkJoin }:

{ name, url, proxy ? null }:

symlinkJoin {
  inherit name;
  paths = [
    (stdenv.mkDerivation {
      inherit name;
      nativeBuildInputs = [ makeWrapper ];
      buildCommand = ''
        makeWrapper ${electron}/bin/electron $out/bin/${name} \
          --set ELECTRON_APP_URL "${url}" \
          --set ELECTRON_APP_NAME "${name}" \
          ${lib.optionalString (proxy != null) ''--set ELECTRON_APP_PROXY "${proxy}"''} \
          --add-flags "${./index.js}"
      '';
    })
    (makeDesktopItem {
      inherit name;
      desktopName = name;
      exec = name;
      type = "Application";
      categories = [ "Network" ];
    })
  ];
}
