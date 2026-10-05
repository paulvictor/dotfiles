{pkgs, ...}:

{
  home.packages = with pkgs; [
    pi-coding-agent
    pi-acp
    nodejs
    claude-code
    claude-agent-acp
    opencode
    qwen-code
  ];
  programs.pi = {
    enable = false;
    # runtimePackages = with pkgs; [ git jq comma ];

#     settings.packages = [
#       "npm:pi-provider-litellm"
#       "npm:pi-proxy-router"
#     ];

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
