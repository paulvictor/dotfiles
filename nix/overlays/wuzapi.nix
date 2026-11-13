final: prev:

{
  wuzapi = final.buildGoModule {
    pname = "wuzapi";
    version = "unstable-2026-11-13";
    src = final.fetchFromGitHub {
      owner = "asternic";
      repo = "wuzapi";
      rev = "845dafab1f203b707b7b4f17cf609b24a4cdd051";
      hash = "sha256-WelLRB8JErEHtQQYpDhv+bUGmtMoy7SLpZi78ySHMoY=";
    };
    vendorHash = "sha256-iO4RWk9GIh9w7CABymdT8BNT0+/pmfMfdMbpgghWvPw=";
    postInstall = ''
      cp -r $src/static $out/bin/
    '';
  };
}
