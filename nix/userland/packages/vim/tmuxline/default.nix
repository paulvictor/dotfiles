{ pkgs, fetchFromGitHub, ... }:

with pkgs;

vimUtils.buildVimPlugin {
  pname = "tmuxline";
  version = "1142333aaef6b542f46ccd5c1e806bdd9382f005";
  src = fetchFromGitHub {
    owner = "edkolev";
    repo = "tmuxline.vim";
    rev = "1142333aaef6b542f46ccd5c1e806bdd9382f005";
    sha256 = "1dr2ndixvnx6ir92kgij66hn29nj0aadbz643ayzf5vra66xichn";
  };
}
