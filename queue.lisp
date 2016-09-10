(in-package :just-player)

(defclass queue ()
  ((queue-mutex    :accessor queue-mutex
                   :initform (make-lock "Player queue lock"))
   (current-source :accessor queue-current-source
                   :initform nil)
   (current-stream :accessor queue-current-stream
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
(defgeneric set-current (queue idx)) ; DO NOT CHANGE CURRENT-SOURCE!!, may not held lock

(defmacro with-current-source (queue &body body)
  (let ((stream-sym (gensym)))
    `(unwind-protect (progn ,@body)
       (with-accessors ((,stream-sym queue-current-stream)) ,queue
         (when ,stream-sym
           (close ,stream-sym)
           (setf ,stream-sym nil
                 (queue-current-source ,queue) nil))))))

(defun set-audio-source (queue pathname start end) ; Must be called with lock held
  "Make audio source based on file extension"
  (let* ((stream (open pathname :element-type '(unsigned-byte 8))) ; FIXME: lock?
         (type (pathname-type pathname))
         (class-type
          (cond
            ((string= "flac" type) 'flac-source)
            ((string= "wv" type) 'wv-source)
            (t (error 'player-error :message "Unknown file type")))))
    (setf (queue-current-stream queue) stream
          (queue-current-source queue)
          (make-instance class-type
                         :stream stream
                         :start start
                         :end end))))

;; For debugging
(defclass empty-queue (queue)
  ())

(defmethod queue-as-list ((queue empty-queue)) nil)
(defmethod next-source ((queue empty-queue)) nil)

;; For testing ;)
(defclass one-file-queue (queue)
  ((filename :reader ofq-filename
             :initarg :filename)))

(defmethod next-source ((queue one-file-queue))
  (if (not (queue-current-source queue))
      ;; Setup a new source
      (with-lock-held ((queue-mutex queue))
        (set-audio-source queue (pathname (ofq-filename queue))
                          nil nil))
      ;; We played one file, no more to give
      ))

(defmethod set-current ((queue one-file-queue) idx)
  (if (/= idx 0) (error 'player-error :message "No such track in queue")))

;; More or less for real use :)
(defclass cue-sheet-queue (queue)
  ((filename :reader cue-filename
             :initarg :filename)
   (index    :accessor cue-index
             :initform 0
             :documentation "Current track index")
   (tree     :accessor cue-tree
             :documentation "Parsed cue sheet tree")
   #+nil
   (timings  :accessor cue-timings)
   (track-info :accessor track-info
               :documentation "Overriden track info")))

(defmethod initialize-instance :after ((queue cue-sheet-queue) &rest args)
  (declare (ignore args))
  (setf (cue-tree queue)
        (parse-cue-helper (cue-filename queue))))

(defmethod next-source ((queue cue-sheet-queue))
  (let ((tree (cue-tree queue)))
    ;; One big "fuck you lisp" lock
    ;; I just can't do this right on lisp, I've tryed
    (with-lock-held ((queue-mutex queue))
      (with-accessors ((current-source queue-current-source)) queue
        (let* ((index (cue-index queue))
               (current-track (get-track-by-idx tree index))
               (next-track (get-track-by-idx tree (1+ index))))
          (cond
            ((not current-source)
             (set-audio-source queue
                               (merge-pathnames
                                (make-pathname :directory
                                               (pathname-directory (pathname (cue-filename queue))))
                                (get-file-name tree current-track))
                               (get-track-index-sec current-track)
                               (if next-track (get-track-index-sec next-track))))
            (current-track
             (setf (interval-start current-source)
                   (get-track-index-sec current-track)
                   (interval-end current-source)
                   (if next-track (get-track-index-sec next-track))))
            (t
             (setf current-source nil)))

          (when current-track
            (incf (cue-index queue))
            (setf (track-info queue)
                  (make-track-info :artist (get-from-toplevel tree :performer)
                                   :album (get-from-toplevel tree :title)
                                   :title (get-from-track current-track :title)))))
      current-source))))

(defmethod current-source-track-info ((queue cue-sheet-queue))
  (if (queue-current-source queue) (track-info queue)))

(defmethod current-source-time-played ((queue cue-sheet-queue))
  (let ((current-source (queue-current-source queue)))
    (if current-source
        (- (floor (sample-counter current-source)
                  (source-samplerate current-source))
           (interval-start current-source)))))

(defmethod current-source-time-total ((queue cue-sheet-queue))
  (let ((current-source (queue-current-source queue)))
    (if current-source
        (- (or (interval-end current-source)
               (floor (source-totalsamples current-source)
                      (source-samplerate current-source)))
           (interval-start current-source)))))

(defmethod set-current ((queue cue-sheet-queue) idx)
  (let* ((tree (cue-tree queue))
         (track (nth idx (second tree))))
    (if (not track)
        (error 'player-error :message "Index is too large, cannot seek"))
    (if (queue-current-source queue)
        (error 'player-error :message "You must stop player first"))
    (setf (cue-index queue) idx)))
