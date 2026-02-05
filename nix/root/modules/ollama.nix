{config, pkgs, lib, ...}:

{
  services.ollama = {
    enable = true;
    environmentVariables = {
      OLLAMA_NUM_GPU = "0";
      OLLAMA_MAX_LOADED_MODELS = "2";
      OLLAMA_KEEP_ALIVE = "120";
      OLLAMA_CONTEXT_LENGTH= toString (64*1024);
    };
    loadModels = [
      # Load it on individual machines
#       "deepseek-r1"
#       "qwen2.5-coder"
#       "deepseek-coder:6.7b"
#       "codegemma"
    ];
  };
  systemd.services.ollama.serviceConfig = {
    MemoryMax = "16G";
  };
  services.open-webui = {
    enable = true;
    port = 3333;
    openFirewall = true;
  };
}
