self: super:
{
  mpd = super.mpd.override {
    flacSupport = true;
    vorbisSupport = true;
    madSupport = true;
    id3tagSupport = true;
    ffmpegSupport = true;
    aacSupport = true;
    lameSupport = true;
    pulseaudioSupport = true;
    opusSupport = true;
    ffmpeg = super.ffmpeg-full.override {
      nonfreeLicensing = true;
      libopus = super.libopus;
      lame = super.lame;
      libogg = super.libogg;
    };
  };
}
