(in-package :just-player)

;; General definitions
(defclass audio-source ()
  ((stream         :initarg :stream
                   :reader source-stream
                   :documentation "A stream associated with an open audio file")
   (bitreader      :accessor source-reader
                   :documentation "easy-audio's BITREADER")
   (sample-counter :accessor sample-counter
                   :initform 0
                   :documentation "Index of the current sample to be decoded")
   (track-info     :accessor track-info
                   :initform (make-track-info)))
  (:documentation "General audio source class"))

(defgeneric initialize-track-info (source)
  (:documentation "Initialize track-info slot based on metadata"))
(defgeneric prepare-decoder (source)
  (:documentation "PREPARE-DECODER is called before the first data block is decoded."))
(defgeneric data-available-p (source)
  (:method-combination and)
  (:documentation "Returns T if there is more data to be decoded."))
(defgeneric decode-frame (source)
  (:documentation "Return decoded data from the current data block."))
(defgeneric seek (source seconds)
  (:documentation "Make a data block beginning from SECONDS second the current."))
(defgeneric source-samplerate (source))
(defgeneric source-samplesize (source))
(defgeneric source-channels (source))
(defgeneric source-blocksize (source)
  (:documentation "A native block size for this source."))
(defgeneric source-totalsamples (source))

(defclass time-interval ()
  ((start :accessor interval-start
          :initarg :start
          :initform nil)
   (end   :accessor interval-end
          :initarg :end
          :initform nil))
  (:documentation "Time interval class with two slots: START and END"))

;; TIME-INTERVAL methods
(defmethod data-available-p and ((source time-interval))
  (if (interval-end source)
      (< (sample-counter source)
         (* (interval-end source)
            (source-samplerate source)))
      t))

(defmethod prepare-decoder :after ((source time-interval))
  (if (interval-start source)
      (seek source (interval-start source))))

;; General AUDIO-SOURCE methods
#+nil
(defmethod initialize-instance :after ((source audio-source) &rest opts)
  (declare (ignore opts))
  (initialize-track-info source))

(defmethod seek :around ((source audio-source) seconds)
  (let ((sample (floor (* seconds (source-samplerate source)))))
    (call-next-method source sample)
    (setf (sample-counter source) sample)))

(defmethod data-available-p and ((source audio-source))
    (< (+ (sample-counter source)
          (source-blocksize source))
       (source-totalsamples source)))

;; FLAC
(defclass flac-source (audio-source time-interval)
  ((metadata      :accessor flac-metadata)))

(flet ((get-streaminfo (flac-source)
         (the flac:streaminfo (first (flac-metadata flac-source)))))
  (define-getters (flac-source get-streaminfo)
    (source-samplerate   . flac:streaminfo-samplerate)
    (source-samplesize   . flac:streaminfo-bitspersample)
    (source-channels     . flac:streaminfo-channels)
    (source-blocksize    . flac:streaminfo-minblocksize)
    (source-totalsamples . flac:streaminfo-totalsamples)))

(defmethod initialize-track-info ((source flac-source))
  (let ((vorbis-comment (find 'flac:vorbis-comment (flac-metadata source)
                              :key #'type-of)))
    (if vorbis-comment
        (let ((track-info (track-info source))
              (parsed-comments (mapcar #'key/val-from-vorbis-comment
                                       (flac:vorbis-user-comments vorbis-comment))))
          (setf (track-info-artist track-info)
                (cdr (assoc :artist parsed-comments))
                (track-info-album track-info)
                (cdr (assoc :album parsed-comments))
                (track-info-title track-info)
                (cdr (assoc :title parsed-comments)))))))

(defmethod initialize-instance :after ((source flac-source) &rest args)
  (declare (ignore args))
  (handler-bind
      ((flac:flac-bad-metadata
        (lambda (c) (invoke-restart 'flac:skip-malformed-metadata c))))
    (setf (source-reader source)
          (flac:open-flac (source-stream source))
          (flac-metadata source)
          (flac:read-metadata (source-reader source))))
  (let ((streaminfo (first (flac-metadata source))))
    (declare (type flac:streaminfo streaminfo))
    (if (/= (flac:streaminfo-minblocksize streaminfo)
            (flac:streaminfo-maxblocksize streaminfo))
        (error 'player-error :message "Variable block size")))
  (initialize-track-info source))

(defmethod prepare-decoder ((source flac-source))
  (let ((streaminfo (first (flac-metadata source))))
    (declare (type flac:streaminfo streaminfo))
    (setq flac:*out-buffers* (flac:make-output-buffers streaminfo))))

(defmethod decode-frame ((source flac-source))
  (let ((streaminfo (first (flac-metadata source))))
    (declare (type flac:streaminfo streaminfo))
    (let ((frame (flac:read-frame (source-reader source)
                                  streaminfo)))
      (incf (sample-counter source)
            (flac:frame-block-size frame))
      (flac:frame-decode frame))))

(defmethod seek ((source flac-source) sample)
  (let ((streaminfo (first (flac-metadata source))))
    (declare (type flac:streaminfo streaminfo))
    (flac:seek-sample (source-reader source)
                      sample
                      :streaminfo streaminfo
                      :seektable (flac:metadata-find-seektable
                                  (flac-metadata source)))))

;; WavPack
(defclass wv-source (audio-source time-interval)
  ((reference-block :accessor wv-reference-block)))

(defmethod initialize-instance :after ((source wv-source) &rest args)
  (declare (ignore args))
  (let ((reader (wv:open-wv (source-stream source))))
    (wv:restore-sync reader)
    (setf (source-reader source) reader
          (wv-reference-block source)
          (wv:read-wv-block reader))
    (bitreader:reader-position reader 0)
    (wv:restore-sync reader)))

 (flet ((get-reference-block (wv-source)
         (wv-reference-block wv-source)))
  (define-getters (wv-source get-reference-block)
    (source-samplerate   . wv:block-samplerate)
    (source-samplesize   . wv:block-bps)
    (source-channels     . wv:block-channels)
    (source-blocksize    . wv:block-block-samples)
    (source-totalsamples . wv:block-total-samples)))

(defmethod prepare-decoder ((source wv-source))
  (setq wv:*residual-buffers* (wv:make-output-buffers
                               (source-reader source))))

(defmethod decode-frame ((source wv-source))
  (let ((wv-block (wv:read-wv-block (source-reader source))))
    (setf (sample-counter source)
          (wv:block-block-index wv-block))
    (wv:decode-wv-block wv-block)))

(defmethod seek ((source wv-source) sample)
  (wv:seek-sample (source-reader source) sample))
