(in-package :just-player)

;; OSS
(defclass oss-backend (audio-backend) ())

(defun guess-sample-format (samplesize)
  "Guess OSS sample format based on sample size"
  (case samplesize
    (8 +afmt-s8+)
    (16 +afmt-s16-le+)
    (32 +afmt-s32-le+)
    (t (error 'player-error
              :message "Wrong audio format"))))

(defmethod configure-parameters ((backend oss-backend)
                                 (source audio-source))
  (with-accessors ((device backend-audio-device)
                   (buffer backend-output-buffer)) backend
    (if (and (slot-boundp backend 'audio-device)
             (open-stream-p device))
        (close device))
    (setf device
          (make-instance 'dsp-device-output
                         :sample-format (guess-sample-format (source-samplesize source))
                         :sample-rate (source-samplerate source)
                         :channels (source-channels source))
          buffer
          (make-array (* (source-channels source)
                         (source-blocksize source))
                      :element-type '(signed-byte 32))))
  (source-blocksize source))

(defmethod write-data-frame ((backend oss-backend)
                             (source audio-source))
  (write-sequence
   (utils:mixchannels
    (backend-output-buffer backend)
    (decode-frame source))
   (backend-audio-device backend)))

(defmethod close-backend ((backend oss-backend))
  (if (slot-boundp backend 'audio-device) ; KLUDGE
      (close (backend-audio-device backend))))
