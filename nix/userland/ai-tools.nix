{pkgs, ...}:

{
  home.packages = with pkgs; [
    # pi-coding-agent is provided by programs.pi (wrapped) below
    pi-acp
    nodejs
    claude-code
    claude-agent-acp
    opencode
    qwen-code
  ];
  programs.pi = {
    enable = true;
    # runtimePackages = with pkgs; [ git jq comma ];

    packages = [
      "npm:pi-proxy-router@1.2.3"
      "npm:@billjr99/pi-openai-compat@1.1.34"
      "npm:pi-provider-litellm@4.0.1"

      # Claude Code-style modes:
      #   /mode yolo|auto|approve|strict — permission gating for bash calls
      "npm:@zhushanwen/pi-permission@1.6.0"
      #   /plan toggles Build <-> Plan (read-only) with approval handoff
      "npm:@pixelsnis/pi-plan-mode@0.1.4"
    ];

#     # providers = {
# #       xyne = {
# #       };
# #     };

#     extensions = {
#       ripgrep-search.enable = true;
#       subagents.enable = true;
#       lazy-archify.enable = true;
#       image-tools.enable = true;
#       app-screenshot.enable = true;
#       plan-mode = {
#         enable = true;
#         mode = "balanced";
#       };
#     };

#     skills = {
#       commit-style.enable = true;
#       generative-ui.enable = true;
#     };
  };
}
