self: super:
{
  julia_15 = self.stdenv.mkDerivation rec {
    pname = "julia_bin";
    version = "1.5.2";

    src = self.fetchurl {
      url = "https://julialang-s3.julialang.org/bin/linux/x64/1.5/julia-${version}-linux-x86_64.tar.gz";
      # Use `nix-prefetch-url` to get the hash.
      sha256 = "0c26b11qy4csws6vvi27lsl0nmqszaf7lk1ya0jrg8zgvkx099vd";
    };

    nativeBuildInputs = [ self.autoPatchelfHook ];

    # Stripping the shared libraries breaks dynamic loading.
    dontStrip = true;

    installPhase = ''
      mkdir -p $out
      tar -x -C $out -f $src --strip-components 1
      # Lacks a string table, so we are unable to patch it.
      rm $out/lib/julia/libccalltest.so.debug
      # Patch for pre-compilation as the Nix store file time stamps are pinned to the start of the epoch.
      sed -i 's/\(ftime != trunc(ftime_req, digits=6)\)$/\1 \&\& ftime != 1.0/' $out/share/julia/base/loading.jl
      grep '&& ftime != 1.0$' $out/share/julia/base/loading.jl > /dev/null || exit 1
    '';

    meta = with self.stdenv.lib; {
      description =
        "High-level performance-oriented dynamical language for technical computing";
      homepage = "https://julialang.org";
      license = licenses.mit;
      platforms = platforms.linux;
    };
  };
}
