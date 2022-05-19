{ pkgs  ? import <nixpkgs> {}}:

with pkgs;
let
  goodbye-ads-hosts = builtins.fetchurl {
    url = https://raw.githubusercontent.com/jerryn70/GoodbyeAds/87a8902d971eeebffcf9dda8e8cd6853bfe5ab42/Hosts/GoodbyeAds.txt;
    sha256 = "1ljgqd6kr4sizx2812lrk0i4m0b2bvnkmi3mb8jjaliqlvw3892q";
  };
  yt-ads-hosts = builtins.fetchurl {
    url = https://raw.githubusercontent.com/jerryn70/GoodbyeAds/87a8902d971eeebffcf9dda8e8cd6853bfe5ab42/Extension/GoodbyeAds-YouTube-AdBlock.txt;
    sha256 = "03lkv523bsqsiwxdxh6vhac3zw4m025inm9ma2ah9qs9yp6d2kja";
  };
  stevenblack-hosts = builtins.fetchurl {
    url = https://raw.githubusercontent.com/StevenBlack/hosts/0adb22b4bffc948168dbd7e25e8e2055b5ddfd75/data/StevenBlack/hosts;
    sha256 = "0gr5br069vadhxg9v13fp2pif261xg4dipjlhgh50gkb16dddxa0";
  };
in # concatText "blacklist-hosts" [ goodbye-ads-hosts yt-ads-hosts stevenblack-hosts ]

  runCommandLocal "blacklist.txt"
    { executable = false; meta = {}; destination = ""; files = [ goodbye-ads-hosts yt-ads-hosts stevenblack-hosts ]; checkPhase = "";}
      ''
        file=$out$destination
        mkdir -p "$(dirname "$file")"
        cat $files  | grep -v clickhouse.com > "$file"
      ''
