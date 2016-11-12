(defpackage just-player-commands)
(in-package :just-player)

(defvar *interface-stream*
  (make-two-way-stream *standard-input* *standard-output*))

;; (defvar *command-table* (make-hash-table))

(defun read-argument (type)
  (declare (type (member :string :number) type))
  (let ((line (read-line *interface-stream*)))
    (case type
      (:number (parse-integer line))
      (:string line))))

(defmacro defcommand (command-name arguments options &body body)
  (let ((command-symbol (gensym))
        (lambda-arguments (mapcar #'first arguments))
        (lambda-types (mapcar #'second arguments))
        (prompt (cdr (find :prompt options :key #'car)))
        (interactivep (find :interactive-arguments options :key #'car)))
    `(let ((,command-symbol (intern (symbol-name ',command-name)
                                    (find-package :just-player-commands))))
       (setf (get ,command-symbol :command)
             (lambda ,lambda-arguments
               (declare
                ,@(loop
                     for type in lambda-types
                     for arg in lambda-arguments
                     collect
                       `(type ,type ,arg)))
               ,@body)

             (get ,command-symbol :prompt)
             (lambda ()
               ,@(if (and (= (length prompt) 1)
                          (stringp (car prompt)))
                     `((write-line ,(car prompt) *interface-stream*))
                     prompt)
               (force-output *interface-stream*))

             (get ,command-symbol :interactive-arguments)
             ,(if interactivep
                  `(lambda ()
                     (list
                      ,@(loop
                           for type in lambda-types
                           collect
                             `(read-argument ,(intern (symbol-name type)
                                                      (find-package :keyword)))))))))))

(defun invoke-command (name)
  (declare (type string name))
  (let ((command (find-symbol (string-upcase name)
                              (find-package :just-player-commands))))
    
    (if command
        (handler-case
            (let ((prompt (get command :prompt))
                  (interactive-arguments (get command :interactive-arguments))
                  (func (get command :command)))
              (if prompt (funcall prompt))
              (if interactive-arguments
                  (apply func (funcall interactive-arguments))
                  (funcall func))
              t)
          ((or type-error parse-error) () ())))))

(defun command-loop ()
  (tagbody :loop
     (print-status *interface-stream*)
     (format *interface-stream* " > ")
     (force-output *interface-stream*)
     (let ((line (read-line *interface-stream*)))
       (if (not (invoke-command line))
           (format *interface-stream* "Error executing command~%")))
     (terpri *interface-stream*)
     (go :loop)))

(defcommand pause () ()
  (pause))

(defcommand stop () ()
  (stop))

(defcommand quit () ()
  (stop)
  (sb-ext:exit))

(defcommand play-track
    ((track-num number))
  ((:prompt
    (print-queue *interface-stream*)
    (format *interface-stream* "> ")
    (force-output *interface-stream*))
   (:interactive-arguments))
  (play-track track-num))

(defcommand print-status () ()
  (print-status *interface-stream*))

(defcommand print-queue () ()
  (print-queue *interface-stream*))
