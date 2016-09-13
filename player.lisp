(in-package :just-player)

(defclass player ()
  ((thread :accessor player-thread)
   (mutex  :accessor player-mutex
           :initform (make-lock "Player thread lock"))
   (convar :accessor player-condvar
           :initform (make-condition-variable))
   (state  :accessor player-state
           :initform :stop
           :type player-state)
   (queue  :accessor player-queue
           :initform (make-instance 'empty-queue)))
  (:metaclass singleton)
  (:documentation "Singleton player class"))

(deftype player-state () `(member :stop :paused :playing))

(defun player-thread-alive-p (player)
  "If player is playing or paused"
  (and (slot-boundp player 'thread)
       (thread-alive-p (player-thread player))))

(defun stop ()
  "Stop the player"
  (let ((player (make-instance 'player)))
    (with-lock-held ((player-mutex player))
      (with-accessors ((state player-state)) player
        (case (the player-state state)
          (:playing (setf state :stop))
          (:paused
           (setf state :stop)
           (condition-notify (player-condvar player))))))
    (if (player-thread-alive-p player)
        (join-thread (player-thread player))))
  :stop)

(defun pause ()
  "Toggle pause"
  (let ((player (make-instance 'player)))
    (with-lock-held ((player-mutex player))
      (with-accessors ((state player-state)) player
        (case (the player-state state)
          (:stop :stop)
          (:playing (setf state :paused))
          (:paused (setf state :playing)
                   (condition-notify (player-condvar player))
                   :playing))))))

(defun play-body ()
  "Player thread loop"
  (let* ((player (make-instance 'player))
         (queue (player-queue player))
         (current-source (queue-current-source queue))
         state)
    (with-audio-backend (backend oss-backend)
      (with-current-source queue
        (block player-loop
          (loop
             for next-source = (with-lock-held ((player-mutex player))
                                 (next-source queue))
             while next-source
             do
               (when (not (eq next-source current-source))
                 (if (/= (configure-parameters backend next-source)
                         (source-blocksize next-source))
                     (error 'player-error :message "Cannot set native block size"))
                 (prepare-decoder next-source))

               (loop
                  while (data-available-p next-source)
                  do
                    (with-lock-held ((player-mutex player))
                      (setq state (the player-state (player-state player)))
                      (when (eq state :paused)
                        (flush-buffers backend)
                        (condition-wait
                         (player-condvar player)
                         (player-mutex player))
                        (setq state (the player-state (player-state player)))))
                    (if (eq state :stop) (return-from player-loop nil))
                    (write-data-frame backend next-source))

               (setq current-source next-source)))))))

(defun error-handler (c)
  (princ c)
  (terpri)
  ;; Continue if we can
  (continue)
  ;; Set player state
  (setf (player-state (make-instance 'player)) :stop)
  ;; And let it crash
  ;; Backend and source will be closed automatically
  (format t "Can't handle error, terminating player thread"))

(defun play (&key queue idx)
  "Play an audio FILE, optionally starting on START second end ending on END"
  (let* ((player (make-instance 'player))
         (state (player-state player)))
    (declare (type player-state state))
    (when (member state '(:playing :paused))
      (stop)
      (return-from play (play :queue queue :idx idx)))
    (if queue (setf (player-queue player) queue))
    (if idx (set-current (player-queue player) idx))
    (setf
     (player-state player) :playing
     (player-thread player)
     (make-thread
      (lambda ()
        (handler-bind
            (((or player-error
                  flac:flac-error
                  wv:wavpack-error)
             #'error-handler))
          (play-body)))
      :name "Player thread")))
  :playing)

(defun play-cue (filename &key idx)
  "Helper for playing cue sheets"
  (play :queue (make-instance 'cue-sheet-queue :filename filename)
        :idx idx))

(defun play-single (filename)
  "Helper for playing single files"
  (play :queue (make-instance 'one-file-queue :filename filename)))

(defun play-directory (dirname)
  "Helper for playing directories"
  (play :queue (make-instance 'directory-queue :directory dirname)))

(defun play-track (idx)
  "Helper for playing a track with index IDX"
  (play :idx idx))

(defun seconds=>string (seconds)
  (multiple-value-bind (minutes rem)
      (floor seconds 60)
    (format nil "~d:~d" minutes rem)))

(deftype status-line-element ()
  '(or string (member :time-played :time-total :state
                      :artist :album :title)))

#+nil
(defmacro case-with-let (keyform &body lets-and-cases)
  (let ((block-sym (gensym)))
  `(block ,block-sym
     ,@(loop for item in lets-and-cases collect
           (destructuring-bind (let-form . body) item
             (let ((case-form
                    (loop for (case% . statements) in body collect
                         `(if (eql ,case% ,keyform) (return-from ,block-sym (progn ,@statements))))))
               `(let ,let-form ,@case-form)))))))

(defun make-status-printer (format-list)
  (lambda (stream)
    (let* ((player (make-instance 'player))
           (queue (player-queue player)))
      (let-with-lock ((player-mutex player)
                      ((state (player-state player))
                       (track-info (current-source-track-info queue))
                       (time-played (current-source-time-played queue))))
          (flet ((process-item (format-item)
                   (declare (type status-line-element format-item))
                   (if (stringp format-item) format-item
                       (case format-item
                         (:state state)
                         (:time-played (if time-played (seconds=>string time-played)))
                         (:time-total (if track-info (seconds=>string
                                                      (track-info-time-total track-info))))
                         (:artist (if track-info (track-info-artist track-info)))
                         (:album (if track-info (track-info-album track-info)))
                         (:title (if track-info (track-info-title track-info)))))))
            (format stream "~{~a~^ ~}" (mapcar #'process-item format-list)))
          state))))

(defun print-status (&optional (stream *standard-output*))
  (let ((status-printer (make-status-printer '(:state :artist
                                               "-" :title :time-played
                                               "/" :time-total))))
    (funcall status-printer stream)))

(defun print-queue (&optional (stream *standard-output*))
  (print (queue-as-list (player-queue (make-instance 'player))) stream))
