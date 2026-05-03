# overlays/nyxt4.nix
final: prev:

let
  extracted = builtins.fetchTarball {
    url = "https://github.com/atlas-engineer/nyxt/releases/download/4.0.0/Linux-Nyxt-x86_64.tar.gz";
    sha256 = "sha256:0ygqkx90gi8i8a49mmq4a1nsa4i3xwsdlx27a2h4zv2z610axlm0";
  };

  desktopItem = final.makeDesktopItem {
    name = "nyxt";
    desktopName = "Nyxt";
    exec = "nyxt %U";
    categories = [ "Network" "WebBrowser" ];
  };

  nyxt4 = final.appimageTools.wrapType2 {
    pname = "nyxt";
    version = "4.0.0";
    src = "${extracted}/Nyxt-x86_64.AppImage";
    extraPkgs = pkgs: with pkgs; [ xdg-utils timidity ];
    nativeBuildInputs = [final.makeWrapper];
    extraInstallCommands = ''
      wrapProgram $out/bin/nyxt --set APPIMAGE_EXTRACT_AND_RUN 1
      cp -r ${desktopItem}/share $out/share
    '';
  };

in

{inherit nyxt4;}

