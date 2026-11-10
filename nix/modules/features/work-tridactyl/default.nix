# Work shortcuts for tridactyl (appended to the tridactylrc of the `firefox` feature)
{
  flake.modules.homeManager.work-tridactyl = {
    xdg.configFile."tridactyl/tridactylrc".text = ''
      bind c fillcmdline_notrail tabopen https://bitbucket.juspay.net/dashboard
    '';
  };
}
