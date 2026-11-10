final: prev:

{
  wuzapi = final.buildGoModule {
    pname = "wuzapi";
    version = "unstable-2026-10-09";
    src = final.fetchFromGitHub {
      owner = "asternic";
      repo = "wuzapi";
      rev = "67f894f166c37216dd45ddd493e1bef07404bb59";
      hash = "sha256-jGde9jtesQ2FkKcAclOC5mvmNpKlxT/6E1dJr4/PoQY=";
    };
    vendorHash = "sha256-yVabUqo3tWkd9YE6+djYp1os3ji9weT3Xe8nAKfy744=";
    postInstall = ''
      cp -r $src/static $out/bin/
    '';
  };
}
