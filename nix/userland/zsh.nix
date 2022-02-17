{ config, pkgs, lib, specialArgs, ... }:

{
  programs.zsh = import ./config/zsh.nix { inherit pkgs config; };
  xdg.configFile."zsh/themes/spaceship.zsh-theme".source =
    let src = pkgs.fetchFromGitHub {
      "owner" = "denysdovhan";
      "repo" = "spaceship-prompt";
      "rev" = "d9f25e14e7bec4bef223fd8a9151d40b8aa868fa";
      "sha256" = "0vl5dymd07mi42wgkh0c3q8vf76hls1759dirlh3ryrq6w9nrdbf";
    }; in "${src.out}/spaceship.zsh-theme";
  xdg.configFile."zsh/plugins/zsh-completions/zsh-completions.plugin.zsh".source =
    let src = pkgs.fetchFromGitHub {
      "owner" = "zsh-users";
      "repo" = "zsh-completions";
      "rev" = "5dd73237d598563e03ea8e84ff9deb6a6ed70848";
      "sha256" = "1yf4rz99acdsiy0y1v3bm65xvs2m0sl92ysz0rnnrlbd5amn283l";
    }; in "${src.out}/zsh-completions.plugin.zsh";
}
