self: super:
{
   pass = super.pass.overrideAttrs (oldAttrs: {
     buildInputs = [ self.makeWrapper ];
     postFixup = oldAttrs.postFixup + ''
       wrapProgram $out/bin/pass \
         --set PASSWORD_STORE_GPG_OPTS "--no-throw-keyids"
     '';
  });
}
