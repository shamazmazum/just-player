(defpackage just-player
  (:use #:cl #:cl-oss #:bordeaux-threads #:cue-parser)
  (:export #:play
           #:stop
           #:pause
           #:print-status))
