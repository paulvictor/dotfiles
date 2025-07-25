{config, ...}:

{
  imports = [
    ../../services/admin.nix
    # https://evilmartians.com/chronicles/stick-with-security-yubikey-ssh-gnupg-macos
      #     ../services/gpg-agent.nix
      #     ../services/ln-ssh-auth-sock.nix
      #     ../services/tuns.nix
    ../../services/power-mgmt.nix
  ];
  nixpkgs.hostPlatform = "aarch64-darwin";
  networking.hostName = "anarki";
  networking.computerName = "Anarki";
  networking.knownNetworkServices = ["Wi-Fi" "Ethernet Adaptor" "Thunderbolt Ethernet"];
  users.users."paul.victor" = {
    name = "paul.victor";
    home = "/Users/paul.victor";
  };
}
