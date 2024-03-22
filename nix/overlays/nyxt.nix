final: prev:
{
  nyxt = prev.nyxt.src.override {
    src = final.fetchFromGitHub {
      owner = "atlas-engineer";
      repo = "nyxt";
      rev = "3.11.3";
      hash = "sha256-KkVn2sTvEYD9OYPezlckMdhMvQ2LKETwmsn+P1Ti424=";
    };
  }
}
