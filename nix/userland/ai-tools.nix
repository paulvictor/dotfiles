{pkgs, ...}:

{
  home.packages = with pkgs; [
    gemini-cli
    claude-agent-acp
    qwen-code
  ];
}
