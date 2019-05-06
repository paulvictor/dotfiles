self: super:

let
  pinentry-gtk2-only =
    (self.pinentry.override({ enabledFlavors = ["gtk2"];})).overrideAttrs(oldAttrs: {
      buildInputs = oldAttrs.buildInputs ++ [ self.git ];
      postInstall = "unlink $out/bin/pinentry";
      outputs = [ "out" ];
    });
  pinentry-curses-only =
    (self.pinentry.override({ enabledFlavors = ["curses"];})).overrideAttrs(oldAttrs: {
      buildInputs = oldAttrs.buildInputs ++ [ self.git ];
      postInstall = "unlink $out/bin/pinentry";
      outputs = [ "out" ];
    });
  binPath =
    with super;
    lib.makeBinPath [ cryptsetup gettext gnupg pinentry-gtk2-only utillinux e2fsprogs git ];
in
{
  tomb = super.tomb.overrideAttrs(oldAttrs: {
    buildInputs =
      with super;
      oldAttrs.buildInputs ++ [lsof cryptsetup git];
    installPhase = ''
      install -Dm755 tomb       $out/bin/tomb
      install -Dm644 doc/tomb.1 $out/share/man/man1/tomb.1

      wrapProgram $out/bin/tomb \
        --prefix PATH : $out/bin:${binPath} \
        --prefix SUDO_ASKPASS : ${super.x11_ssh_askpass}/libexec/x11-ssh-askpass
    '';
  });
}


