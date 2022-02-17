{ pkgs ? import <nixpkgs> {}}:

with pkgs;
buildGoModule rec {
  pname = "golinks";
  version = "0.0.5";

  src = fetchgit {
    url = "https://git.mills.io/prologic/golinks.git";
    rev = "2c5a102ee9066e76d1811307342f765874fa0c98";
    sha256 = "sha256-6ovm+1rreYYysD+ipWJY7OKeOGC/A57BYX6pSwCfluc=";
  };

  vendorSha256 = "sha256-UGx8KB2oaky4tVIT+7p5N5+7faMZZFBHkND2ZVGFARk=";

  proxyVendor = true;

}
