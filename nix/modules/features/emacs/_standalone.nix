# Emacs bundled with its own config and fonts, for `nix run` on machines
# without the stowed ~/.emacs.d
{ lib, stdenv, emacs, symlinkJoin, makeWrapper, makeFontsConf
, fonts, glibcLocales }:

let
  # Extends the host's fontconfig (/etc/fonts/conf.d, ~/.local/share/fonts, ...)
  fontsConf = makeFontsConf {
    fontDirectories = fonts;
  };
  # builtins.path dereferences the emacs.d symlink; "${./emacs.d}" would copy the dangling link
  initDir = builtins.path { path = ./emacs.d; name = "emacs.d"; };
in
symlinkJoin {
  name = "emacs-standalone";
  meta.mainProgram = "emacs";
  paths = [ emacs ];
  nativeBuildInputs = [ makeWrapper ];
  # State goes to PERSIST_DIR (see init.el), so a read-only init dir is fine
  postBuild = ''
    rm $out/bin/emacs
    makeWrapper ${lib.getExe emacs} $out/bin/emacs \
      --add-flags --init-directory=${initDir} \
      --set FONTCONFIG_FILE ${fontsConf} \
      ${lib.optionalString stdenv.hostPlatform.isLinux
        "--set-default LOCALE_ARCHIVE ${glibcLocales}/lib/locale/locale-archive"}
  '';
}
