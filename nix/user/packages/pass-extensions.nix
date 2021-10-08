self: super:
{
    pass-with-extensions = super.pass.withExtensions(e: [ e.pass-otp e.pass-tomb e.pass-import e.pass-update ]);
#    pass = super.pass.overrideAttrs (oldAttrs: {
#      buildInputs = oldAttrs.buildInputs ++ [ self.makeWrapper ];
#      postFixup = oldAttrs.postFixup + ''
#        wrapProgram $out/bin/pass \
#          --set PASSWORD_STORE_GPG_OPTS "--no-throw-keyids"
#      '';
#   });
}
