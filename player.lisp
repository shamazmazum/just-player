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
         (current-source (queue-current-source queue)))
    (with-audio-backend (backend oss-backend)
      (with-current-source queue
        (loop for next-source = (next-source queue)
           while next-source do
             (when (not (eq next-source current-source))
               (if (/= (configure-parameters backend next-source)
                       (source-blocksize next-source))
                   (error 'player-error :message "Cannot set native block size"))
               (prepare-decoder next-source))

             (loop
                with state = :playing
                while (and (data-available-p next-source)
                           (eq state :playing)) do
                  (write-data-frame backend next-source)
                  (with-lock-held ((player-mutex player))
                    (setq state (the player-state (player-state player)))
                    (when (eq state :paused)
                      (condition-wait
                       (player-condvar player)
                       (player-mutex player))
                      (setq state (the player-state (player-state player))))))

             (setq current-source next-source))))
    (with-lock-held ((player-mutex player))
      (setf (player-state player) :stop))))

(defun error-handler (c)
  (declare (ignore c))
  ;; Set player state
  (setf (player-state (make-instance 'player)) :stop)
  ;; And let it crash
  ;; Backend and source will be closed automatically
)

(defun play (&key queue idx)
  "Play an audio FILE, optionally starting on START second end ending on END"
  (let* ((player (make-instance 'player))
         (state (player-state player)))
    (declare (type player-state state))
    (when (member state '(:playing :paused))
      (stop)
      (return-from play (play :queue queue :idx idx)))
    (if queue (setf (player-queue player) queue))
    (if idx (set-current queue idx))
    (setf
     (player-state player) :playing
     (player-thread player)
     (make-thread
      (lambda ()
        (handler-bind
            ((player-error #'error-handler))
          (play-body)))
      :name "Player thread")))
  :playing)

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


;; Needs to be remade
(defun make-status-printer (format-list)
  (lambda (source stream)
    (let ((player (make-instance 'player)))
      (flet ((process-item (format-item)
               (declare (type status-line-element format-item))
               (if (stringp format-item) format-item
                   (case format-item
                     (:state (player-state player))
                     (:time-played
                      (seconds=>string
                       (floor (sample-counter source)
                              (source-samplerate source))))
                     (:time-total
                      (seconds=>string
                       (floor (source-totalsamples source)
                              (source-samplerate source))))
                     (:artist (track-info-artist (track-info source)))
                     (:album (track-info-album (track-info source)))
                     (:title (track-info-title (track-info source)))))))
        (format stream "~{~a~^ ~}" (mapcar #'process-item format-list))))))

(defun print-status (&optional (stream *standard-output*))
  (let ((stop-status-printer (make-status-printer '(:state)))
        (play/pause-status-printer (make-status-printer '(:state :artist
                                                          "-" :title :time-played
                                                          "/" :time-total)))
        (player (make-instance 'player))
        state source)
    (with-lock-held ((player-mutex player))
      (setq state (player-state player)
            source (if (not (eq state :stop))
                       (queue-current-source (player-queue player))))) ; FIXME: queue lock?
    (funcall (if source play/pause-status-printer stop-status-printer) source stream)
    state))
