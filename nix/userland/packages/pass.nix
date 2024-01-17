self: super:

let
  pass_ = super.pass.overrideAttrs (oldAttrs: {
    postFixup = oldAttrs.postFixup + ''
      wrapProgram $out/bin/pass \
        --set PASSWORD_STORE_GPG_OPTS "--no-throw-keyids"
    '';
  });
  with-extensions = super.pass.withExtensions(e: [ e.pass-otp e.pass-import e.pass-update ])
in
{
   pass-with-extensions = with-extensions;
   pass = with-extensions;
}
