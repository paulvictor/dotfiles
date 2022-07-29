args@{ config, lib, pkgs, ... } :

with pkgs;
let
  goodbye-ads-hosts = "${args.specialArgs.goodbyeAds}/Hosts/GoodbyeAds.txt";
  yt-ads-hosts = "${args.specialArgs.goodbyeAds}/Extension/GoodbyeAds-YouTube-AdBlock.txt";
  stevenblack-hosts = "${args.specialArgs.stevenBlack}/data/StevenBlack/hosts";
  fullBlacklist = runCommandLocal "blacklist.txt"
    { executable = false; meta = {}; destination = ""; files = [ goodbye-ads-hosts yt-ads-hosts stevenblack-hosts ]; checkPhase = "";}
    ''
      file=$out$destination
      mkdir -p "$(dirname "$file")"
      cat $files | grep -v clickhouse.com | sed "/^[^#]/s/.*:.*//" > "$file"
    '';
in

{
  networking.extraHosts =
    builtins.readFile fullBlacklist;
}
