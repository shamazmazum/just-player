(in-package :just-player)

(defclass queue ()
  ((current-source :accessor queue-current-source
                   :initform nil)
   (current-stream :accessor queue-current-stream
                   :initform nil)
   (index          :accessor queue-index
                   :initform 0
                   :documentation "Current track index")))

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

(defgeneric queue-as-list (queue))
(defgeneric next-source (queue))
(defgeneric set-current (queue idx)) ; DO NOT CHANGE CURRENT-SOURCE!!

(defmethod set-current ((queue queue) idx)
  (if (queue-current-source queue)
      (error 'player-error :message "You must stop player first"))
  (setf (queue-index queue) idx))

(defmacro with-current-source (queue &body body)
  (let ((stream-sym (gensym))
        (player-sym (gensym)))
    `(unwind-protect (progn ,@body)
       (let ((,player-sym (make-instance 'player)))
         (with-accessors ((,stream-sym queue-current-stream)) ,queue
           (with-lock-held ((player-mutex ,player-sym))
             (when ,stream-sym
               (close ,stream-sym)
               (setf ,stream-sym nil
                     (queue-current-source ,queue) nil))
             ;; Also set player state to :stop
             (setf (player-state ,player-sym) :stop)))))))

(defun guess-source-type (pathname)
  (let ((extension (pathname-type (pathname pathname))))
    (cond
      ((string= "flac" extension) 'flac-source)
      ((string= "wv" extension) 'wv-source)
      ((string= "wa" extension) 'wa-source)
      ((string= "ape" extension) 'ape-source)
      (t (error 'player-error :message "Unknown file type")))))

(defun set-audio-source (queue pathname &optional start end)
  "Make audio source based on file extension"
  (let ((class-type (guess-source-type pathname))
        (stream (open pathname :element-type '(unsigned-byte 8)))) ; FIXME: lock?
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
      (set-audio-source queue (pathname (ofq-filename queue)))
      ;; We played one file, no more to give
      ))

(defmethod set-current ((queue one-file-queue) idx)
  (if (/= idx 0) (error 'player-error :message "No such track in queue")))

;; More or less for real use :)
(defclass cue-sheet-queue (queue)
  ((filename   :reader cue-filename
               :initarg :filename)
   (source-filename :accessor cue-source-filename)
   (time-list  :accessor cue-time-list)
   (track-list :accessor cue-track-list)))

(defun cuesheet-from-apev2 (tags)
  (let ((cuesheet (find "Cuesheet" tags
                        :key #'ape:apev2-tag-item-key
                        :test #'string-equal)))
    (if cuesheet
        (with-input-from-string (stream (ape:apev2-tag-item-value cuesheet))
          (parse-cue stream))
        (error 'player-error :message "APEv2 tag does not contain a cuesheet"))))

(defun read-tree (pathname)
  (let* ((pathname (pathname pathname))
         (type (pathname-type pathname)))
    (cond
      ((string= "cue" type)
       (parse-cue-file pathname))
      ((string= "wv" type)
       (with-open-file (stream pathname :element-type '(unsigned-byte 8))
         (let* ((reader (bitreader:make-reader :stream stream))
                (tag (ape:read-apev2-tag-from-end reader)))
           (cuesheet-from-apev2 tag))))
      (t (error 'player-error :message "Unknown cue sheet container")))))

(defmethod initialize-instance :after ((queue cue-sheet-queue) &rest args)
  (declare (ignore args))
  (let* ((tree (read-tree (cue-filename queue)))
         (source-filename (merge-pathnames
                           (make-pathname
                            :directory (pathname-directory (pathname (cue-filename queue))))
                           (get-file-name tree (get-track-by-idx tree 0))))
         (artist (get-command-arg (car tree) :performer))
         (album (get-command-arg (car tree) :title))
         (total-time
          (with-open-file (stream source-filename :element-type '(unsigned-byte 8))
            (let ((source (make-instance (guess-source-type source-filename) :stream stream)))
              (track-info-time-total (track-info source))))))
    (setf (cue-source-filename queue) source-filename)
    (let ((summary-list
           (loop
              with tracks = (second tree)
              for track in tracks
              for idx from 0 by 1 collect
                (let ((start-time (get-track-index-sec track))
                      (end-time (if (< (1+ idx) (length tracks))
                                    (or
                                     (get-track-index-sec (nth (1+ idx) tracks) :pregap)
                                     (get-track-index-sec (nth (1+ idx) tracks) :start))
                                    total-time)))
                  (cons (cons start-time end-time)
                        (make-track-info :artist artist
                                         :album album
                                         :title (get-command-arg track :title)
                                         :time-total (- end-time start-time)))))))
      (setf (cue-time-list queue) (mapcar #'car summary-list)
            (cue-track-list queue) (mapcar #'cdr summary-list)))))

(defmethod next-source ((queue cue-sheet-queue))
  (with-accessors ((current-source queue-current-source)
                   (index queue-index)) queue
    (let ((current-time (nth index (cue-time-list queue)))
          (current-track (nth index (cue-track-list queue))))

      (if (not current-source)
          (set-audio-source queue (cue-source-filename queue)))

      (cond
        (current-track
         (setf (interval-start current-source)
               (car current-time)
               (interval-end current-source)
               (cdr current-time)
               (track-info current-source)
               current-track)
         (incf index))
        (t
         (setf current-source nil)))
      current-source)))

(defmethod current-source-time-played ((queue cue-sheet-queue))
  (let ((current-source (queue-current-source queue)))
    (if current-source
        (- (call-next-method)
           (interval-start current-source)))))

(defmethod set-current ((queue cue-sheet-queue) idx)
  (let ((track (nth idx (cue-track-list queue))))
    (if (not track)
        (error 'player-error :message "Index is too large, cannot seek")))
  (call-next-method))

(defmethod queue-as-list ((queue cue-sheet-queue))
  (cue-track-list queue))

(defclass directory-queue (queue)
  ((directory   :reader queue-directory
                :initarg :directory
                :initform (error "Specify directory"))
   (index       :accessor directory-index
                :initform 0
                :documentation "Current track index")
   (file-list   :accessor directory-file-list)
   (track-infos :accessor directory-track-infos)))

(defmethod initialize-instance :after ((queue directory-queue) &rest args)
  (declare (ignore args))
  (let ((file-list (remove #'directory-pathname-p
                           (list-directory (queue-directory queue)))))
    (flet ((populate-list (filename list)
             (restart-case
                 (with-open-file (stream filename :element-type '(unsigned-byte 8))
                   (let ((source (make-instance (guess-source-type filename) :stream stream)))
                     (cons (cons filename (track-info source))
                           list)))
               (continue ()
                 :report "Skip unreadable file and continue"
                 list))))
      (let ((audio-list (reduce #'populate-list file-list
                                :from-end t
                                :initial-value nil)))
        (setf (directory-file-list queue)
              (mapcar #'car audio-list)
              (directory-track-infos queue)
              (mapcar #'cdr audio-list))))))

(defmethod next-source ((queue directory-queue))
  (let ((file-list (directory-file-list queue)))
    (with-accessors ((current-source queue-current-source)
                     (index directory-index)) queue

      (when current-source
        (close (queue-current-stream queue))
        (setf current-source nil))

      (let ((file-name (nth index file-list)))
        (when file-name
          (incf index)
          (set-audio-source queue file-name nil nil)))
      current-source)))

(defmethod set-current ((queue directory-queue) idx)
  (if (>= idx (length (directory-file-list queue)))
      (error 'player-error :message "Index is too large, cannot seek"))
  (call-next-method))

(defmethod queue-as-list ((queue directory-queue))
  (directory-track-infos queue))
