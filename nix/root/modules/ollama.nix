{config, pkgs, lib, ...}:

{
  services.ollama = {
    environmentVariables = {
      OLLAMA_NUM_GPU = "0";
      OLLAMA_MAX_LOADED_MODELS = "2";
      OLLAMA_KEEP_ALIVE = "120";
      OLLAMA_CONTEXT_LENGTH= toString (64*1024);
    };
    loadModels = [];
  };
  systemd.services.ollama.serviceConfig = {
    MemoryMax = lib.mkDefault "16G";
  };
  services.open-webui = {
    port = 3333;
    openFirewall = true;
  };
}
