{
  perSystem = { pkgs, ... }: {
    devShells.default = pkgs.mkShell {
      packages = with pkgs; [
        sops
        just
        ssh-to-age
        nvd
        nix-diff
      ];
    };
  };
}
