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

(defun set-audio-source (queue pathname start end)
  "Make audio source based on file extension"
  (let* ((type (pathname-type pathname))
         (class-type
          (cond
            ((string= "flac" type) 'flac-source)
            ((string= "wv" type) 'wv-source)
            (t (error 'player-error :message "Unknown file type"))))
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
      (set-audio-source queue (pathname (ofq-filename queue))
                        nil nil)
      ;; We played one file, no more to give
      ))

(defmethod set-current ((queue one-file-queue) idx)
  (if (/= idx 0) (error 'player-error :message "No such track in queue")))

;; More or less for real use :)
(defclass cue-sheet-queue (queue)
  ((filename :reader cue-filename
             :initarg :filename)

   (tree     :accessor cue-tree
             :documentation "Parsed cue sheet tree")))

(defmethod initialize-instance :after ((queue cue-sheet-queue) &rest args)
  (declare (ignore args))
  (setf (cue-tree queue)
        (parse-cue-helper (cue-filename queue))))

(defmethod next-source ((queue cue-sheet-queue))
  (let ((tree (cue-tree queue)))
    (with-accessors ((current-source queue-current-source)) queue
      (let* ((index (queue-index queue))
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
          (incf (queue-index queue))
          (let ((track-info (track-info current-source)))
            (setf (track-info-artist track-info)
                  (get-from-toplevel tree :performer)
                  (track-info-album track-info)
                  (get-from-toplevel tree :title)
                  (track-info-title track-info)
                  (get-from-track current-track :title)
                  (track-info-time-total track-info)
                  (- (or (interval-end current-source)
                         (track-info-time-total track-info))
                     (interval-start current-source))))))
      current-source)))

(defmethod current-source-time-played ((queue cue-sheet-queue))
  (let ((current-source (queue-current-source queue)))
    (if current-source
        (- (floor (sample-counter current-source)
                  (source-samplerate current-source))
           (interval-start current-source)))))

(defmethod set-current ((queue cue-sheet-queue) idx)
  (let* ((tree (cue-tree queue))
         (track (nth idx (second tree))))
    (if (not track)
        (error 'player-error :message "Index is too large, cannot seek")))
  (call-next-method))

(defmethod queue-as-list ((queue cue-sheet-queue))
  (let* ((tree (cue-tree queue))
         (tracks (second tree))
         (artist (get-from-toplevel tree :performer))
         (album (get-from-toplevel tree :title)))
    (mapcar (lambda (track)
              (make-track-info :artist artist
                               :album album
                               :title (get-from-track track :title)))
            tracks)))

(defclass directory-queue (queue)
  ((directory :reader queue-directory
              :initarg :directory
              :initform (error "Specify directory"))
   (index     :accessor directory-index
              :initform 0
              :documentation "Current track index")
   (file-list :accessor directory-file-list)))

(defmethod initialize-instance :after ((queue directory-queue) &rest args)
  (declare (ignore args))
  (setf (directory-file-list queue)
        (remove #'directory-pathname-p
                (list-directory (queue-directory queue)))))

(defmethod next-source ((queue directory-queue))
  (let ((file-list (directory-file-list queue)))
    (with-accessors ((current-source queue-current-source)
                     (index directory-index)) queue

      (tagbody choose-next
         (when current-source
           (close (queue-current-stream queue))
           (setf current-source nil))

         (let ((file-name (nth index file-list)))
           (when file-name
             (incf index)
             (restart-case (set-audio-source queue file-name nil nil)
               (continue ()
                 :report "Skip unreadable file and continue"
                 (go choose-next))))))
      current-source)))

(defmethod set-current ((queue directory-queue) idx)
  (if (>= idx (length (directory-file-list queue)))
      (error 'player-error :message "Index is too large, cannot seek"))
  (call-next-method))
