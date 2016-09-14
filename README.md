just-player
===========

`just-player` is a simple audio player based on `easy-audio` library. It can only play
audio files in FLAC and WavPack formats by now. It also has cue sheets support through
`cue-parser` library. It does not support playlists by now, but has different type of
queues.

Dependencies
------------
 * `cue-parser` (https://github.com/shamazmazum/cue-parser)
 * `easy-audio` (https://github.com/shamazmazum/easy-audio)
 * Bordeaux threads
 * `cl-fad`

How to use
----------

Load the player as a system: `(asdf:load-system :just-player)`. Now you can play audio
using different types of queues, for example play all audio files (of supported format) in
a directory invoking `(just-player:play-directory #p"/path/to/dir/")` (note a trailing
slash) or use cue sheet as a playlist: `(just-player:play-cue #p"/path/to/cue")`. No
matter which queue you use, you can print current player status by
`(just-player:print-status)` or content of current queue by
`(just-player:print-queue)`. You can toggle pause by `(just-player:pause)` and stop the
player by `(just-player:stop)`. You can choose a track from queue for playing by giving
its index to `play-track`: `(just-player:play-track idx)`.
