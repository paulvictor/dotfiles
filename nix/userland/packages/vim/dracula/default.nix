{ pkgs }:

with pkgs;

vimUtils.buildVimPlugin {
  pname = "dracula";
  version = "f90b5825db827914be6ccb6a351fe4c20998ec9c";
  src = fetchFromGitHub {
    "owner" = "dracula";
    "repo" = "vim";
    "rev" = "f90b5825db827914be6ccb6a351fe4c20998ec9c";
    "sha256" = "0hwj4w1dy511dfvpl5xq89d0cqsnmzv2gq78l3nim09jbn16hkhr";
  };
}
