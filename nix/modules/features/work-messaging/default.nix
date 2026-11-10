# Work chat apps as electron apps. Needs `electron-apps`, and `gp-vpn` for the proxy behind the VPN.
{
  flake.modules.homeManager.work-messaging = { config, ... }:
    let
      socks = config.custom.gpVpn.socks;
    in
    {
      programs.electronApps = {
        JuspaySlack.url = "https://juspay.slack.com/sign_in_with_password";
        XyneSpaces = {
          url = "https://spaces.xyne.juspay.net";
          proxy = "socks5://${socks.host}:${toString socks.port}";
        };
      };
    };
}
