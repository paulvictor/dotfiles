{pkgs, ...}:

{
  home.packages = with pkgs; [
    gemini-cli
    claude-code-acp
    qwen-code
  ];
}
