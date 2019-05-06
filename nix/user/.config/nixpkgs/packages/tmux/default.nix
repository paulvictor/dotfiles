{pkgs ? import <nixpkgs> {}}:

with pkgs;
let
  tmuxConf = import ./mkTmuxConfig.nix { inherit pkgs; };
in
runCommand "mytmux" { buildInputs = [ makeWrapper ]; } ''
  mkdir -pv $out/bin
  makeWrapper ${tmux}/bin/tmux $out/bin/tmux \
    --add-flags -f \
    --add-flags ${tmuxConf} \
    --add-flags -2 \
    --set TMUX_TMPDIR /tmp/tmux
''
