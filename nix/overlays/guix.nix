final: prev:
{
  guix = prev.stdenv.mkDerivation rec
    { name = "guix-${version}";
      version = "1.4.0";

      src = final.fetchurl {
        url = "https://ftp.gnu.org/gnu/guix/guix-binary-${version}.${prev.stdenv.targetPlatform.system}.tar.xz";
        sha256 = {
          "x86_64-linux" = "sha256-I2ynycWVix85bCkk/MW8nW/evLG0zzx8bUbUv2YO2ck=";
          "aarch64-linux" = "1khh129kcadz07cb4mam90p50wahp2wqswdc4dhn6narrwv6aks4";
        }."${prev.stdenv.targetPlatform.system}";
      };
      sourceRoot = ".";

      outputs = [ "out" "store" "var" ];
      phases = [ "unpackPhase" "installPhase" ];

      installPhase = ''
      # copy the /gnu/store content
      mkdir -p $store
      cp -r gnu $store

      # copy /var content
      mkdir -p $var
      cp -r var $var

      # link guix binaries
      mkdir -p $out/bin
      ln -s /var/guix/profiles/per-user/root/current-guix/bin/guix $out/bin/guix
      ln -s /var/guix/profiles/per-user/root/current-guix/bin/guix-daemon $out/bin/guix-daemon
    '';

      meta = with final.lib; {
        description = "The GNU Guix package manager";
        homepage = https://www.gnu.org/software/guix/;
        license = licenses.gpl3Plus;
        maintainers = [ maintainers.johnazoidberg ];
        platforms = [ "aarch64-linux" "i686-linux" "x86_64-linux" ];
      };

    };
}
