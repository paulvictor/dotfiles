{config, pkgs, lib, ...}:

{
  services.ollama = {
    enable = true;
    loadModels = [
#       "codellama"
#       "devstral"
#       "deepseek-r1"
    ];
  };
}
