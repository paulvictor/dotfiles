{pkgs, ...}:

let
  claude-agent-acp = pkgs.writeShellScriptBin "claude-agent-acp" ''
    exec tee /tmp/claude-acp-stdin.log \
      | ${pkgs.claude-agent-acp}/bin/claude-agent-acp 2>/tmp/claude-acp-stderr.log \
      | tee /tmp/claude-acp-stdout.log
  '';
in
{
  home.packages = with pkgs; [
    gemini-cli
    claude-agent-acp
    opencode
    qwen-code
  ];
}
