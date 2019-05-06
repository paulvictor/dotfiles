self: super:

{
  urxvt_perls = super.urxvt_perls.overrideDerivation (oldattrs: {
    version = "1f4897ccdd9b7ddc848dda8919d3a4e0186bf535";
    installPhase = builtins.replaceStrings [ "clipboard" "url-select" ] [ "" "" ] oldattrs.installPhase;
    src = super.fetchFromGitHub {
      owner = "muennich";
      repo = "urxvt-perls";
      rev = "1f4897ccdd9b7ddc848dda8919d3a4e0186bf535";
      sha256 = "0xvwfw7965ghhd9g6rl6y6fgpd444l46rjqmlgg0rfjypbh6c0p1";
    };
  });
}
