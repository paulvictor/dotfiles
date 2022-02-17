self: super:

{
  xdotool = super.xdotool.overrideAttrs(old: rec {
    version = "3.20210804.2";

    buildInputs =
      old.buildInputs ++
      (with super.xorg; [
        libXext
      ]);

    src = self.fetchurl {
      url = "https://github.com/jordansissel/xdotool/releases/download/v${version}/xdotool-${version}.tar.gz";
      sha256 = "12bc8qbzqbnckm3jwwi091r2wxc7k4d59k3qrc71xjbqb5dv3rpx";
    };

  });
}
