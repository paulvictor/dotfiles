{ pkgs }:

let
  quri-patch = ./local-patches/quicklisp-quri.patch;
#   quri-patch = pkgs.fetchpatch {
#     url = https://gist.github.com/paulvictor/b1629314f2ad8fbe67a3e8424039e6be/raw/37a821846da3c314c68fc8023abd5d75244a2be9/quicklisp-quri.patch;
#     sha256 = "sha256-j21lZidxrDoU7V0+Qztpt+MsAjswUwsQ7ZdUbogIDwE=";
#   };
  vieb-9-1-patch = pkgs.fetchpatch {
    url = https://github.com/NixOS/nixpkgs/pull/184119.patch;
    sha256 = "sha256-jJv/ZBwtEP3tv7PK12cW9CSyPqFp2IT1T7DffNiCUtk=";
  };
  quicklisp-22-07-08-patch = pkgs.fetchpatch {
    url = https://github.com/NixOS/nixpkgs/pull/191961.patch;
    sha256 = "sha256-h3y7ZCyTvmNIN6s47RZwCrsJpmaYxDaUUFPXyLfMA5o=";
  };
in
[ quri-patch vieb-9-1-patch quicklisp-22-07-08-patch ]