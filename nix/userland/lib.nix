pkgs:
let
  lib = pkgs.lib;
in
rec {
  writeGuileScheme =
    { f ? "main", extraArgs ? [ "--no-auto-compile" ] }:
      let scriptArgs =
        lib.concatStringsSep " "
          ([ "-e" f ] ++ extraArgs  ++ [ "-s" ]);
      in
        pkgs.writers.makeScriptWriter {
        interpreter =
          ''
            ${pkgs.guile}/bin/guile \
            ${scriptArgs}
            !#
          '';
        };

  # from https://github.com/LumiGuide/lumi-example/blob/master/nix/lib.nix
  callPackagesFromDir = self : dir :
    pkgs.lib.genAttrs (builtins.attrNames (builtins.readDir dir))
      (name : self.callPackage (dir + "/${name}") {});

  # Returns a derivation with the given name that copies the contents
  # of the given directory to $out.
  copyDir = name : dir : pkgs.runCommand name { inherit dir; } ''
    cp -rT $dir $out
  '';

  escapeConfiguratorStr = lib.replaceChars
                            [ "\$"   "\\"   "\""   ]
                            [ "\$\$" "\\\\" "\\\"" ];

  # Function that builds an .iso image containing a NixOS Live-CD
  # configured with the supplied list of modules.
  isoImage = modules : (getConfig modules).system.build.isoImage;

  # Turns a module into a machine configuration
  getConfig = modules :
    (import (pkgs.path + /nixos)
            { configuration = { imports = modules; }; }
    ).config;

  fetchS3 = {path, sha256, accessKeyId, secretAccessKey, region}:
    pkgs.runCommand (baseNameOf path) {
      inherit accessKeyId secretAccessKey region path;
      outputHashMode = "flat";
      outputHashAlgo = "sha256";
      outputHash     = sha256;
    } ''
      AWS_ACCESS_KEY_ID="$accessKeyId" \
      AWS_SECRET_ACCESS_KEY="$secretAccessKey" \
      AWS_DEFAULT_REGION="$region" \
      ${pkgs.awscli}/bin/aws s3 cp "s3://$path" "$out"
    '';
}

