self: super:
{
  ffmpeg-full = super.ffmpeg-full.override {
    nonfreeLicensing = true;
    inherit (super) libopus lame libogg;
  };
}

