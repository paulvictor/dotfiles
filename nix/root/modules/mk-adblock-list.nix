{ pkgs  ? import <nixpkgs> {}}:

with pkgs;
let
  goodbye-ads-hosts = builtins.fetchurl {
    url = https://raw.githubusercontent.com/jerryn70/GoodbyeAds/a780eb7563af945b3364d5fcbca37dcafbb26da8/Hosts/GoodbyeAds.txt;
    sha256 = "15d3w6sp7xfsbala1rbhgk6y9hv2f6cbpswhnrg6sqfffi6gfrdi";
  };
  yt-ads-hosts = builtins.fetchurl {
    url = https://raw.githubusercontent.com/jerryn70/GoodbyeAds/a780eb7563af945b3364d5fcbca37dcafbb26da8/Extension/GoodbyeAds-YouTube-AdBlock.txt;
    sha256 = "0yys784g49biwwg4xdzzivkhdxi5mhw9d7fs0c660x9fm616va9p";
  };
  stevenblack-hosts = builtins.fetchurl {
    url = https://raw.githubusercontent.com/StevenBlack/hosts/0adb22b4bffc948168dbd7e25e8e2055b5ddfd75/data/StevenBlack/hosts;
    sha256 = "0gr5br069vadhxg9v13fp2pif261xg4dipjlhgh50gkb16dddxa0";
  };
in concatText "blacklist-hosts" [ goodbye-ads-hosts yt-ads-hosts stevenblack-hosts ]
