(in-package :just-player)

;; General definitions
(defclass audio-backend ()
  ((output-buffer :accessor backend-output-buffer)
   (audio-device :accessor backend-audio-device))
  (:documentation "General audio backend class"))

(defgeneric configure-parameters (backend source)
  (:documentation "Configure backend parameters, such as samplerate, samplesize,
channels etc."))
(defgeneric write-data-frame (backend source)
  (:documentation "Write a small amount of audio data to audio device. This amount
is defined in CONFIGURE-PARAMETERS call and is both backend and source dependant."))
(defgeneric close-backend (backend)
  (:documentation "Close backend. Must be called when backend is no longer needed."))
(defgeneric flush-buffers (backend)
  (:documentation "Flush buffers before long pause")
  (:method ((backend audio-backend))
    (declare (ignore backend))
    t))

(defmacro with-audio-backend ((name type) &body body)
  `(let ((,name (make-instance ',type)))
     (unwind-protect
          (progn ,@body)
       (close-backend ,name))))

;; OSS
(defclass oss-backend (audio-backend) ())

(defmethod flush-buffers ((backend oss-backend))
  (force-output (backend-audio-device backend)))

(defun guess-sample-format (samplesize)
  "Guess OSS sample format based on sample size"
  (case samplesize
    (8  :afmt-s8
    (16 :afmt-s16-le)
    (32 :afmt-s32-le)
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
