self: super:
{
  ffmpeg-full = super.ffmpeg-full.override {
    # Not really used non free formats for a while now
    # Has to be removed
    nonfreeLicensing = true;
  };
}

