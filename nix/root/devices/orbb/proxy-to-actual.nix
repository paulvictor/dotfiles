{ pkgs, config, lib, ...}:

{
  services.nginx.virtualHosts."accounts.paulvictor.xyz" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      basicAuthFile = "/var/lib/nginx/creds.htpass";
      proxyPass = "http://localhost:5006/";
      extraConfig = ''
        # rewrite ^/accounts(.*)$ $1 break;
        # rewrite_log on;
        proxy_http_version 1.1;
        proxy_cache_bypass $http_upgrade;

        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
      '';
    };
  };
}
