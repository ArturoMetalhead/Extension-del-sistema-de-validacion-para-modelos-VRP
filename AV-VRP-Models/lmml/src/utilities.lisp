(in-package :lmml)

;;;{{{ symb and flatten

(defun mkstr (&rest args)
  "Returns a string with the concatenation of the args"
  (string-upcase
   (with-output-to-string (s)
     (dolist (a args) (princ a s)))))

(defun symb (&rest args)
  "Returns a symbol formed by the concatenation of the args."
  (values (intern (apply #'mkstr args))))

(defun flatten (x)
  "Flattens a structure."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;;;}}}

(defun make-keyword (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

;;;{{{ with-gensym
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym (symbol-name ',s))))
                   syms)
     ,@body))
;;;}}}

(defun format-boxed (stream format-str &rest format-args)
  (let* ((string-to-print
          (if format-args
              (apply 'format `(nil ,format-str ,@format-args))
              (funcall 'format nil format-str)))
         (length (cl:+ 2 (length string-to-print)))
         (=-line (make-string length :initial-element #\=)))
    (format stream "~a~% ~a~%~a~2%"
            =-line string-to-print =-line)))

(setf (symbol-function 'bformat) #'format-boxed)

(defun symbol-downcase (symbol)
  (values (intern (string-downcase (symbol-name symbol)))))

(defmethod length ((obj list))
  (cl:length obj))

(defmethod length ((obj string))
  (cl:length obj))

(defmethod length ((obj symbol))
  (cl:length (symbol-name obj)))

(defmethod length ((obj (eql nil)))
  0)

(defmethod length ((obj number))
  (cl:length (mkstr obj)))
