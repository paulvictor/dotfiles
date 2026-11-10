final: prev:
let
  src = final.fetchFromGitHub {
    owner = "rvaiya";
    repo = "warpd";
    rev = "01650eabf70846deed057a77ada3c0bbb6d97d6e";
    hash = "sha256-61+kJvOi4oog0+tGucc1rWemdx2vp15wlluJE+1PzTs=";
  };
in
{
  warpd = (prev.warpd.overrideAttrs({inherit src;})).override({withX = false;});
}
