final: prev:

{
  xsecurelock = prev.xsecurelock.overrideAttrs(_: {
    postInstall = ''
      wrapProgram $out/libexec/xsecurelock/saver_blank --prefix PATH : ${final.coreutils}/bin --set XSECURELOCK_COMPOSITE_OBSCURER 0
    '';
  });
}
