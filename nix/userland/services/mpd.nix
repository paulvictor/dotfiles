{pkgs, config, ...}:

{
  services.mpd.enable = true;
  services.mpd.dataDir = "${config.home.homeDirectory}/.mpd";
  services.mpd.dbFile = "${config.home.homeDirectory}/.mpd/dbFile";
  services.mpd.musicDirectory = "${config.home.homeDirectory}/Music";
  services.mpd.network.listenAddress = "any";
  services.mpd.extraConfig = ''
    audio_output {
        type                    "fifo"
        name                    "my_fifo"
        path                    "/tmp/mpd.fifo"
        format                  "44100:16:2"
    }
    audio_output {
      type  "pulse"
      name "pulseaudio"
    }

    audio_output {
      type  "httpd"
      name  "My HTTP Stream"
      #encoder  "vorbis"  # optional, vorbis or lame
      port  "8000"
      quality  "5.0"   # do not define if bitrate is defined
      # bitrate  "128"   # do not define if quality is defined
      format  "44100:16:1"
      max_clients "0"   # optional 0=no limit
    }

    restore_paused "yes"
  '';
}
