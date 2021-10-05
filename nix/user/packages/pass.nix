self: super:

{
   pass-with-extensions = super.pass.withExtensions(e: [ e.pass-otp e.pass-tomb e.pass-import ]);
}
