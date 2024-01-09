{ pkgs, config, lib, ...}:

{
  services.nginx.virtualHosts."paulvictor.xyz".locations."/accounts" = {
    basicAuthFile = "/var/lib/nginx/creds.htpass";
    proxyPass = "http://localhost:5006";
    extraConfig = ''
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header Host $host;
    '';
  };
}
