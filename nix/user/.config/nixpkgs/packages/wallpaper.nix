self: super:
{
  # Find more at https://wallpaperplay.com/board/dark-metal-wallpapers
  arch-wallpaper = self.fetchurl {
    url = "https://wallpapercave.com/wp/NOFQh9F.png";
    sha256 = "0n9f4wb4z01ynv6zhi0r0m9hv3fax3vldbrhc8s90y9hwxn7nkn5";
  };
  metallic-wallpaper = self.fetchurl {
    url = "https://wallpaperplay.com/walls/full/f/f/a/267108.jpg";
    sha256 = "0g795mnawc38v94wswqhw3laxvn4nbj1c6g2lhpp9lx2i81v6bcj";
  };
  wall1 = ./wallpapers/267108.jpg;
  wall2 = ./wallpapers/170823.jpg;
  wall3 = ./wallpapers/508419.jpg;
}
