(defsystem :just-player
    :description "Simple audio player based on easy-audio library"
    :maintainer "Vasily Postnicov <shamaz.mazum at gmail.com>"
    :version "0.1"
    :depends-on (:cl-oss :easy-audio :bordeaux-threads :cue-parser :cl-fad)
    :serial t
    :components ((:file "package")
                 (:file "general")
                 (:file "sources")
                 (:file "backends")
                 (:file "queue")
                 (:file "player")))
