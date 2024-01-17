self: super:
rec {
    pass-with-extensions = super.pass.withExtensions(e: [ e.pass-otp e.pass-update ]);
    pass = pass-with-extensions;
}
