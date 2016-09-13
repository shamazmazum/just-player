(in-package :just-player)

(define-condition player-error ()
  ((message :initarg :message
            :reader error-message))
  (:report (lambda (c s)
             (format s "Player error: ~a"
                     (error-message c)))))

(defstruct track-info
  artist album title time-total)

(defun key/val-from-vorbis-comment (comment)
  (declare (type string comment))
  (let ((eq-pos (position #\= comment)))
    (if (not eq-pos) (error 'player-error :message "Not a vorbis comment"))
    (cons
     (intern (string-upcase (subseq comment 0 eq-pos))
             (find-package :keyword))
     (subseq comment (1+ eq-pos)))))

(defmacro define-getters ((class function) &rest getters)
  "Define getters for certain audio parameters"
  (let ((object (gensym "OBJ-")))
    `(progn
         ,@(loop for (getter-name . accessor) in getters collect
               `(defmethod ,getter-name ((,object ,class))
                  (,accessor (,function ,object)))))))

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

(defmethod make-instances-obsolete ((class singleton))
  (setf (singleton-instance class) nil))

(defmacro let-with-lock ((place let-form) &body body)
  (let ((let-list (mapcar #'car let-form))
        (setq-list (reduce (lambda (list acc)
                             (cons
                              (first list)
                              (cons (second list) acc))) let-form
                              :from-end t :initial-value nil)))
    `(let ,let-list
       (with-lock-held (,place)
         (setq ,@setq-list))
       ,@body)))
