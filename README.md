# ytaudio

# TODO

Approximately ordered:

* verbose mode: add logging;
* put generator name in the feed;
* fill in ID3 tags in MP3s;
* provide link to an icon in the feed (`yt-dlp -j -I 0 https://www.youtube.com/channel/id --list-thumbnails`);
* cancel streaming when the client interrupts the response;
* address code FIXMEs;
* how to use `Sem r` inside the response `Conduit`?
* address code TODOs;
* signed URLs for MP3s;
* response header for MP3's last modified time;
* support concurrent MP3 downloads;
* health endpoint: return program, ffmpeg, yt-dlp versions;
* log the number of sent bytes in a response;
* handle no internet/connectivity errors;
* entry filters?
