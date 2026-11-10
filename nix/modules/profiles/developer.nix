# Development tools: shell, editor, git and language tooling, signing keys, cross-arch builds
{
  profiles.developer = {
    nixos = [ "nix-dev" "emulation" "gpg-yubikey" ];
    home = [ "shell" "ssh" "dev" "emacs" "pass" "schemesh" "gpg-yubikey" ];
  };
}
