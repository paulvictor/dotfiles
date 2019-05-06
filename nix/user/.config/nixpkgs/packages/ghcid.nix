{ pkgs }:

with pkgs;

let orig = vimUtils.buildVimPlugin {
  pname = "ghcid";
  version = "692df0f9a712c92f7ed48993693bfca1e5d87288";
  src = fetchFromGitHub {
    owner = "ndmitchell";
    repo = "ghcid";
    rev = "692df0f9a712c92f7ed48993693bfca1e5d87288";
    sha256 = "0661i147grqin8pgb5bv88y85dxwcn0fkzrm190d0f9q479hwrnj";
  };
}; in
orig // { rtp = "${orig.rtp}/plugins/nvim"; }
