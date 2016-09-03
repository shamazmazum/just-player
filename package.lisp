(defpackage just-player
  (:use #:cl #:cl-oss #:bordeaux-threads)
  (:export #:play
           #:stop
           #:pause))
