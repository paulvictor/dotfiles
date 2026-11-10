# Overlays shipped by flake inputs
{ inputs, ... }: {
  flake.overlays = {
    emacs-overlay = inputs.emacsOverlay.overlay;
    kanata-layer = inputs.kanata-layer.overlays.default;
    keyfreq = inputs.keyfreq.overlays.default;
    llm-agents = inputs.llm-agents.overlays.shared-nixpkgs;
    nur = inputs.nur.overlays.default;
    vieb = final: _: inputs.vieb-nix.packagesFunc final;
  };
}
