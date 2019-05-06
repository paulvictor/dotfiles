{pkgs ? import <nixpkgs> {}}:

with pkgs;
let
  profile = import ./mkProfile.nix { inherit pkgs; };
  firefox-wrapped = runCommand "firejail-wrapped-firefox"
    { preferLocalBuild = true;
      allowSubstitutes = false;
    }
    ''
      mkdir -p $out/bin
      cat <<_EOF >$out/bin/firefox
      #! ${pkgs.runtimeShell} -e
      exec /run/wrappers/bin/firejail --profile=${profile} --tracelog -- ${lib.getBin firefox}/bin/firefox "\$@"
      _EOF
      chmod 0755 $out/bin/firefox
    '';

in
  firefox-wrapped
