# password-store with extensions, and passdo to type passwords
{
  # `pass` wrapped with the gpg option
  flake.overlays.pass = final: prev: {
    pass = prev.pass.overrideAttrs (oldAttrs: {
      buildInputs = [ final.makeWrapper ];
      postFixup = oldAttrs.postFixup + ''
        wrapProgram $out/bin/pass \
          --set PASSWORD_STORE_GPG_OPTS "--no-throw-keyids"
      '';
    });
  };

  flake.overlays.passdo = import ./_passdo.nix;

  flake.modules.homeManager.pass = { config, pkgs, ... }: {
    home.packages = [ pkgs.passdo ];
    programs.password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (e: [ e.pass-otp e.pass-import e.pass-update ]);
      settings = {
        PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
        PASSWORD_STORE_X_SELECTION = "SECONDARY";
      };
    };
  };
}
