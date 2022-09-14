{ pkgs }:

{program, name}:

let script = pkgs.writeShellScript name program;
in
[
  "/bin/zsh"
  "-c"
  "/bin/wait4path ${script} && exec ${script}"
]
