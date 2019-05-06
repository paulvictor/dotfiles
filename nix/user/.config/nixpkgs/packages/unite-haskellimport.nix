{pkgs}:

with pkgs;

vimUtils.buildVimPlugin {
  pname = "unite-haskellimport";
  version = "440d605";
  src = fetchFromGitHub {
    owner = "ujihisa";
    repo = "unite-haskellimport";
    rev = "440d6051fb71f7cc5d4b4c83f02377bfa48d0acf";
    sha256 = "1qqbb7prm3hyjhzb5hglnyak35gr4dkj34wcklcjg9zxg9qm1mq0";
  };
}
