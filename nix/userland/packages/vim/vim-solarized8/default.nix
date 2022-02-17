{ pkgs }:

with pkgs;

vimUtils.buildVimPlugin {
  pname = "vim-solarized8";
  version = "30fd9196e0ae330a33ca00e255c8392516bc242c";
  src = fetchFromGitHub {
    "owner" = "lifepillar";
    "repo" = "vim-solarized8";
    "rev" = "30fd9196e0ae330a33ca00e255c8392516bc242c";
    "sha256" = "1rc6ldh7c5w5sy9xa4lbz0j28jainqfvdlfni7qc5s2wdc4i1jy9";
  };
}
