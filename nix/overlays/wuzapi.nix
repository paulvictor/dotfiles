final: prev:

{
  wuzapi = final.buildGoModule {
    pname = "wuzapi";
    version = "unstable-2026-06-05";
    src = final.fetchFromGitHub {
      owner = "asternic";
      repo = "wuzapi";
      rev = "e6d58f895dfa85ab080c856b8625b1c8d2b6463f";
      hash = "sha256-OsLl8WOuCqPJ+C+YdXRqTTScopvD1M1VY4uMPYLzsFo=";
    };
    vendorHash = "sha256-nR7MwvGIJl1MGIZDGxE9vCoeUxzKpDGZn3fUEXECZ7I=";
    postInstall = ''
      cp -r $src/static $out/bin/
    '';
  };
}
