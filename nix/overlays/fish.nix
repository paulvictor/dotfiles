final: prev:

{
  fish = prev.fish.overrideAttrs(_: {
    postUnpack = ''
      cp ${final.docker}/share/fish/vendor_completions.d/docker.fish $sourceRoot/share/completions/docker.fish
    '';
  });
}
