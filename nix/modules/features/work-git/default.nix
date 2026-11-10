# Work identity for repositories under a Juspay/ directory
{
  flake.modules.homeManager.work-git = {
    programs.git.includes = [
      {
        condition = "gitdir:Juspay/";
        contents = {
          browseAtRemote = {
            type = "stash";
          };
          user = {
            email = "paul.victor@juspay.in";
          };
        };
      }
    ];
  };
}
