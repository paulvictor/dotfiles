final: prev:

{
  xsecurelock = prev.xsecurelock.overrideAttrs(old: {
    postInstall = final.lib.replaceStrings [ "\n" ] ["--set  XSECURELOCK_COMPOSITE_OBSCURER 0\n" ] xsecurelock.postInstall;
  });
}
