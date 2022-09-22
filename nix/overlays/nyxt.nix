final: prev:

let
  src = final.fetchFromGitHub {
    owner = "atlas-engineer";
    repo = "nyxt";
    rev = "aef5118a7c30c48583b464e2e0c27d4b0060999b";
    sha256 = "e83KmC9APjiYeK9ti+eejW4VZImZ36r4mVHf1RVe5aw=";
  };
  inherit (prev.lispPackages_new) build-asdf-system sbcl sbclPackages;
in
{
  nyxt-3 = build-asdf-system {
    lisp = sbcl;
    pname = "nyxt";
    version = "2022-09-22";

    asds = [ "nyxt" ];
    systems = [ "nyxt" ];

    lispLibs =
      sbclPackages.nyxt.lispLibs ++
      (with sbclPackages; [ cl-cffi-gtk cl-webkit2 mk-string-metrics ]);

    inherit src;


    nativeBuildInputs = [ final.makeWrapper ];
    buildInputs = [
      # needed for GSETTINGS_SCHEMAS_PATH
      final.gsettings-desktop-schemas final.glib final.gtk3

      # needed for XDG_ICON_DIRS
      final.gnome.adwaita-icon-theme
    ];

    buildScript = final.writeText "build-nyxt.lisp" ''
      (require :asdf)
      (asdf:load-system :nyxt/gtk-application)
      (sb-ext:save-lisp-and-die "nyxt" :executable t
                                       #+sb-core-compression :compression
                                       #+sb-core-compression t
                                       :toplevel #'nyxt:entry-point)
    '';

    # Run with WEBKIT_FORCE_SANDBOX=0 if getting a runtime error in webkitgtk-2.34.4
    installPhase = sbclPackages.nyxt.installPhase;
#     installPhase = sbclPackages.nyxt.installPhase + ''
#       rm -v $out/nyxt
#       mkdir -p $out/bin
#       cp -v nyxt $out/bin
#       wrapProgram $out/bin/nyxt \
#         --prefix LD_LIBRARY_PATH : $LD_LIBRARY_PATH \
#         --prefix XDG_DATA_DIRS : $XDG_ICON_DIRS \
#         --prefix XDG_DATA_DIRS : $GSETTINGS_SCHEMAS_PATH \
#         --prefix GIO_EXTRA_MODULES ":" ${pkgs.dconf.lib}/lib/gio/modules/ \
#         --prefix GIO_EXTRA_MODULES ":" ${pkgs.glib-networking}/lib/gio/modules/
#     '';
  };



#   nyxt-gtk = build-asdf-system {
#     inherit (ql.nyxt) pname lisp;
#     version = "2.2.4";

#     lispLibs = ql.nyxt.lispLibs ++ (with ql; [
#       cl-cffi-gtk cl-webkit2 mk-string-metrics
#     ]);

#     src = pkgs.fetchzip {
#       url = "https://github.com/atlas-engineer/nyxt/archive/2.2.4.tar.gz";
#       sha256 = "12l7ir3q29v06jx0zng5cvlbmap7p709ka3ik6x29lw334qshm9b";
#     };

#     nativeBuildInputs = [ pkgs.makeWrapper ];
#     buildInputs = [
#       # needed for GSETTINGS_SCHEMAS_PATH
#       pkgs.gsettings-desktop-schemas pkgs.glib pkgs.gtk3

#       # needed for XDG_ICON_DIRS
#       pkgs.gnome.adwaita-icon-theme
#     ];

#     buildScript = pkgs.writeText "build-nyxt.lisp" ''
#       (require :asdf)
#       (asdf:load-system :nyxt/gtk-application)
#       (sb-ext:save-lisp-and-die "nyxt" :executable t
#                                        #+sb-core-compression :compression
#                                        #+sb-core-compression t
#                                        :toplevel #'nyxt:entry-point)
#     '';

#     # Run with WEBKIT_FORCE_SANDBOX=0 if getting a runtime error in webkitgtk-2.34.4
#     installPhase = ql.nyxt.installPhase + ''
#       rm -v $out/nyxt
#       mkdir -p $out/bin
#       cp -v nyxt $out/bin
#       wrapProgram $out/bin/nyxt \
#         --prefix LD_LIBRARY_PATH : $LD_LIBRARY_PATH \
#         --prefix XDG_DATA_DIRS : $XDG_ICON_DIRS \
#         --prefix XDG_DATA_DIRS : $GSETTINGS_SCHEMAS_PATH \
#         --prefix GIO_EXTRA_MODULES ":" ${pkgs.dconf.lib}/lib/gio/modules/ \
#         --prefix GIO_EXTRA_MODULES ":" ${pkgs.glib-networking}/lib/gio/modules/
#     '';
#   };

#   nyxt = nyxt-gtk;


#   nyxt = prev.lispPackages.nyxt.overrideAttrs(old: {
#     version = "2022-07-29";
#     inherit src;
#     buildInputs = old.buildInputs ++ [ final.makeWrapper ];
#     postInstall = old.postInstall + "\n" + ''
#       wrapProgram $out/bin/nyxt \
#         --set-default WEBKIT_FORCE_SANDBOX 0
#     '';

#   });
}
