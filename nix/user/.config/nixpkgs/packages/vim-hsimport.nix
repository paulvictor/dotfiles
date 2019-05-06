{ pkgs, fetchFromGitHub, ... }:

with pkgs;

vimUtils.buildVimPlugin {
  pname = "vim-hsimport";
  version = "v0.4.4";
  src = fetchFromGitHub {
    owner = "dan-t";
    repo = "vim-hsimport";
    rev = "v0.4.4";
    sha256 = "0ml0vy0zmk8p7sjh6mddlhqiwz568yjjhx3n6sb0gz5zgscgwvnw";
  };
}
