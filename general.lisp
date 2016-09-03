(in-package :just-player)

(define-condition player-error ()
  ((message :initarg :message
            :reader error-message))
  (:report (lambda (c s)
             (format s "Player error: ~a"
                     (error-message c)))))

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

(defmacro with-audio-backend ((name type) &body body)
  `(let ((,name (make-instance ',type)))
     (unwind-protect
          (progn ,@body)
       (close-backend ,name))))

(defclass audio-source ()
  ((stream         :initarg :stream
                   :reader source-stream
                   :documentation "A stream associated with an open audio file")
   (bitreader      :accessor source-reader
                   :documentation "easy-audio's BITREADER")
   (sample-counter :accessor sample-counter
                   :initform 0
                   :documentation "Index of the current sample to be decoded"))
  (:documentation "General audio source class"))

(defclass time-interval ()
  ((start :reader interval-start
          :initarg :start
          :initform nil)
   (end   :reader interval-end
          :initarg :end
          :initform nil))
  (:documentation "Time interval class with two slots: START and END"))

(defmacro define-getters ((class function) &rest getters)
  "Define getters for certain audio parameters"
  (let ((object (gensym "OBJ-")))
    `(progn
         ,@(loop for (getter-name . accessor) in getters collect
               `(defmethod ,getter-name ((,object ,class))
                  (,accessor (,function ,object)))))))

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

;; General AUDIO-SOURCE/AUDIO-BACKEND methods
(defmethod seek :around ((source audio-source) seconds)
  (let ((sample (floor (* seconds (source-samplerate source)))))
    (call-next-method source sample)
    (setf (sample-counter source) sample)))

(defmethod data-available-p and ((source audio-source))
    (< (+ (sample-counter source)
          (source-blocksize source))
       (source-totalsamples source)))

;; Trendy singleton class ;)
(defclass singleton (standard-class)
  ((instance :initform nil
             :accessor singleton-instance)))

(defmethod sb-mop:validate-superclass ((class singleton)
                                       (super standard-class))
  t)

(defmethod make-instance ((class singleton) &rest opts)
  (declare (ignore opts))
    (with-accessors ((instance singleton-instance)) class
      (if instance instance
          (setf instance (call-next-method)))))

;; For debugging
(defgeneric reset-instance (class)
  (:method ((class singleton))
    (setf (singleton-instance class) nil)))
