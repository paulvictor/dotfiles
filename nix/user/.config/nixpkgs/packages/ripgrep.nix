self: super:
let
  ripgreprc = super.writeText "ripgreprc" ''
    --smart-case
    --hidden
    --colors=line:fg:yellow
    --colors=line:style:bold
    --colors=path:fg:green
    --colors=path:style:bold
    --colors=match:fg:black
    --colors=match:bg:yellow
    --colors=match:style:nobold
  '';
in
{
  ripgrep = super.ripgrep.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [ self.makeWrapper ];
    postInstall = oldAttrs.postInstall or "" + ''
      wrapProgram $out/bin/rg \
        --set RIPGREP_CONFIG_PATH ${ripgreprc}
    '';
  });
}
