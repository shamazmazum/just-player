(defpackage just-player
  (:use #:cl
        #:cl-oss
        #:bordeaux-threads
        #:cue-parser
        #:fad)
  (:local-nicknames (:wv        :easy-audio.wv)
                    (:flac      :easy-audio.flac)
                    (:ape       :easy-audio.ape)
                    (:core      :easy-audio.core)
                    (:bitreader :easy-audio.bitreader))
  (:export #:play
           #:play-cue
           #:play-directory
           #:play-single
           #:stop
           #:pause
           #:play-track
           #:print-status
           #:print-queue
           ;; -----
           #:cue-sheet-queue
           #:one-file-queue
           #:directory-queue
           ;; -----
           #:make-track-printer
           #:make-status-printer
           #:*track-printer*
           #:*status-printer*

           #:defcommand
           #:command-loop))
