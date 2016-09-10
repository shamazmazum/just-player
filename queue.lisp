(in-package :just-player)

(defclass queue ()
  ((queue-mutex    :accessor queue-mutex
                   :initform (make-lock "Player queue lock"))
   (current-source :accessor queue-current-source
                   :initform nil)))

;; This and later: must be called with lock held.
;; Bordeaux threads has no means to check that, lol
(defgeneric current-source-track-info (queue)
  (:method ((queue queue))
    (let ((current-source (queue-current-source queue)))
      (if current-source (track-info current-source)))))
(defgeneric current-source-time-played (queue)
  (:method ((queue queue))
    (let ((current-source (queue-current-source queue)))
      (if current-source
          (floor (sample-counter current-source)
                 (source-samplerate current-source))))))
(defgeneric current-source-time-total (queue)
  (:method ((queue queue))
    (let ((current-source (queue-current-source queue)))
      (if current-source
          (floor (source-totalsamples current-source)
                 (source-samplerate current-source))))))

(defgeneric queue-as-list (queue)) ; Must hold lock inside
(defgeneric next-source (queue)) ; Must hold lock inside
(defgeneric set-current (queue idx)) ; DO NOT CHANGE CURRENT-SOURCE!!, may not hold lock

(defmacro with-current-source (queue &body body)
  (let ((source-sym (gensym)))
    `(unwind-protect (progn ,@body)
       (with-accessors ((,source-sym queue-current-source)) ,queue
         (when ,source-sym
           (close-source ,source-sym)
           (setf ,source-sym nil))))))

(defun make-audio-source (stream type start end)
  "Make audio source based on file extension"
  (let ((class-type
         (cond
           ((string= "flac" type) 'flac-source)
           ((string= "wv" type) 'wv-source)
           (t (error 'player-error :message "Unknown file type")))))
    (make-instance class-type
                   :stream stream
                   :start start
                   :end end)))

;; For debugging
(defclass empty-queue (queue)
  ())

(defmethod queue-as-list ((queue empty-queue)) nil)
(defmethod next-source ((queue empty-queue)) nil)

;; For testing ;)
(defclass one-file-queue (queue)
  ((filename :reader ofq-filename
             :initarg :filename)))

(defmethod next-source ((queue one-file-queue)) ;FIXME: lock needed, but not around open
  (if (not (queue-current-source queue))
      ;; Setup a new source
      (let ((source (make-audio-source (open (ofq-filename queue)
                                             :element-type '(unsigned-byte 8))
                                       (pathname-type (pathname (ofq-filename queue)))
                                       nil nil)))
        (with-lock-held ((queue-mutex queue))
          (setf (queue-current-source queue) source)))
      ;; We played one file, no more to give
      ))

(defmethod set-current ((queue one-file-queue) idx)
  (if (/= idx 0) (error 'player-error :message "No such track in queue")))
