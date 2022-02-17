self: super:
{
  ffmpeg-full = super.ffmpeg-full.override {
    nonfreeLicensing = true;
    libopus = super.libopus;
    lame = super.lame;
    libogg = super.libogg;
  };
}

