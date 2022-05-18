final: prev:

let
  src = final.fetchFromGitHub {
    owner = "atlas-engineer";
    repo = "nyxt";
    rev = "425c71b2c124f8b3bac72d54bce184d0afd0ab1d";
    sha256 = "K1WoMRmD0yS6mXGomcC556q66Gbl2Q+6NGAngUeOh4o=";
  };
in
{
  nyxt = prev.lispPackages.nyxt.overrideAttrs(old: {
    version = "2022-05-14";
    inherit src;
    buildInputs = old.buildInputs ++ [ final.makeWrapper ];
    postInstall = old.postInstall + "\n" + ''
      wrapProgram $out/bin/nyxt \
        --set-default WEBKIT_FORCE_SANDBOX 0
    '';

#     installPhase =
#       let
#         gstBuildInputs = with final.gst_all_1; [
#           gstreamer gst-libav
#           gst-plugins-base
#           gst-plugins-good
#           gst-plugins-bad
#           gst-plugins-ugly
#         ];
#         GST_PLUGIN_SYSTEM_PATH_1_0 = final.lib.makeSearchPathOutput "lib" "lib/gstreamer-1.0" gstBuildInputs;
#       in
#       ''
#         mkdir -p $out/share/applications/
#         sed "s/VERSION/$version/" $src/assets/nyxt.desktop > $out/share/applications/nyxt.desktop
#         for i in 16 32 128 256 512; do
#           mkdir -p "$out/share/icons/hicolor/''${i}x''${i}/apps/"
#           cp -f $src/assets/nyxt_''${i}x''${i}.png "$out/share/icons/hicolor/''${i}x''${i}/apps/nyxt.png"
#         done

#         ls $src/bin

#         mkdir -p $out/bin && makeWrapper $src/bin/nyxt $out/bin/nyxt \
#           --prefix GST_PLUGIN_SYSTEM_PATH_1_0 : "${GST_PLUGIN_SYSTEM_PATH_1_0}" \
#           --set-default WEBKIT_FORCE_SANDBOX 0
#           --argv0 nyxt "''${gappsWrapperArgs[@]}"
#     '';
  });
}

#   GST_PLUGIN_SYSTEM_PATH_1_0 = lib.makeSearchPathOutput "lib" "lib/gstreamer-1.0" gstBuildInputs;

#   dontWrapGApps = true;
#   installPhase = ''
#     mkdir -p $out/share/applications/
#     sed "s/VERSION/$version/" $src/lib/common-lisp/nyxt/assets/nyxt.desktop > $out/share/applications/nyxt.desktop
#     for i in 16 32 128 256 512; do
#       mkdir -p "$out/share/icons/hicolor/''${i}x''${i}/apps/"
#       cp -f $src/lib/common-lisp/nyxt/assets/nyxt_''${i}x''${i}.png "$out/share/icons/hicolor/''${i}x''${i}/apps/nyxt.png"
#     done

#     mkdir -p $out/bin && makeWrapper $src/bin/nyxt $out/bin/nyxt \
#       --prefix GST_PLUGIN_SYSTEM_PATH_1_0 : "${GST_PLUGIN_SYSTEM_PATH_1_0}" \
#       --argv0 nyxt "''${gappsWrapperArgs[@]}"
#   '';
