(in-package :just-player)

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
        (error 'player-error :message "Variable block size"))))

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
