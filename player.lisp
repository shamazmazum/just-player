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
   #+nil
   (current-source
    :accessor player-current-source))
  (:metaclass singleton)
  (:documentation "Singleton player class"))

(deftype player-state () `(member :stop :paused :playing))

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
        (join-thread (player-thread player)))))

(defun pause ()
  "Toggle pause"
  (let ((player (make-instance 'player)))
    (with-lock-held ((player-mutex player))
      (with-accessors ((state player-state)) player
        (case (the player-state state)
          (:stop nil)
          (:playing (setf state :paused))
          (:paused (setf state :playing)
                   (condition-notify (player-condvar player))))))))

(defun play-body (file start end)
  "Player thread loop"
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (with-audio-backend (backend oss-backend)
      (let ((source (make-audio-source in (pathname-type (pathname file))
                                       start end)))
        (if (/= (configure-parameters backend source)
                (source-blocksize source))
            (error 'player-error :message "Cannot set native block size"))
        (prepare-decoder source)
        (loop
           with state = :playing
           with player = (make-instance 'player)
           while (and (data-available-p source)
                      (eq state :playing)) do
             (write-data-frame backend source)
             (with-lock-held ((player-mutex player))
               (setq state (the player-state (player-state player)))
               (when (eq state :paused)
                 (condition-wait
                  (player-condvar player)
                  (player-mutex player))
                 (setq state (the player-state (player-state player))))))))))

(defun error-handler (c)
  (declare (ignore c))
  ;; Set player state
  (setf (player-state (make-instance 'player)) :stop)
  ;; And let it crash
  ;; Backend and source will be closed automatically
)

(defun play (file &optional start end)
  "Play an audio FILE, optionally starting on START second end ending on END"
  (let* ((player (make-instance 'player))
         (state (player-state player)))
    (declare (type player-state state))
    (when (member state '(:playing :paused))
      (stop)
      (return-from play (play file start end)))
    (setf
     (player-state player) :playing
     (player-thread player)
     (make-thread
      (lambda ()
        (handler-bind
            ((player-error #'error-handler))
          (play-body file start end)))
      :name "Player thread"))))
