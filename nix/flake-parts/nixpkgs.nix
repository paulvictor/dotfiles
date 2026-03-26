{ withSystem, inputs, lib, self, ... }: {
  perSystem = { system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = builtins.attrValues (self.overlays);
      config = {
        allowUnfreePredicate =
          pkg: builtins.elem (lib.getName pkg)
            [
              "google-chrome"
              "vivaldi"
              "widevine-cdm"
              "ungoogled-chromium"
              "ungoogled-chromium-unwrapped"
              "prl-tools"
              "firefox-devedition-bin"
              "firefox-developer-edition-bin-unwrapped"
              "open-webui"
            ];
      };
    };
  };
}
