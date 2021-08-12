self: super:
{
  compton-git = super.compton-git.overrideAttrs(attrs: {
    version = "97db599fd8726235beae62391724ad26aa0a0856";
    src = self.fetchFromGitHub {
      owner = "yshui";
      repo = "picom";
      rev = "97db599fd8726235beae62391724ad26aa0a0856";
      sha256 = "10wdk4zxk4kb3ibpwapky99zl8z7k5w0b0gyygn2qvd4d67k7hvq";
    };
  });
}
