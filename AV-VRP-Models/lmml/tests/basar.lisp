(in-package :gagm)

(defabsnode binary-operator ()
  ((left-hand) (rigth-hand)))

(defnode add (binary-operator)
  ())

(defnode mult (binary-operator)
  ())

(defnode assignment (binary-operator)
  ()
  :ctr-name assign-to)

(defnode var-reference ()
  ((var-name)))

(defnode var-declaration ()
  ((var-name))
  :ctr-type macro
  :ctr-name new-var
  :ctr-body `(progn 
               (setf ,var-name
                 (var-reference ',var-name))
               (make-instance 'var-declaration
                              :var-name ',var-name)))

(defnode print-node ()
  ((what-to-print))
  :ctr-name bprint)

(defnode program-node ()
  ((instructions))
  :ctr-name bprogram
  :lambda-key &rest
  :ctr-type macro
  :ctr-body (let* ((declared-var (loop for inst in instructions
                                       if (and (listp inst)
                                               (symbolp (first inst))
                                               (equal 'new-var
                                                      (first inst)))
                                       collect (second inst))))
              `(let* ,declared-var
                 (make-instance 'program-node
                                :instructions (list ,@instructions)))))

(defnode basic-language () ())

(defnode common-lisp (basic-language) ())

(defparameter *cl* (common-lisp))

(defmethod generate-code ((obj number) (lang basic-language) stream)
  (format stream "~a" obj))

(defmethod generate-code ((obj string) (lang basic-language) stream)
  (format stream "~a" obj))

(defmethod generate-code ((obj symbol) (lang basic-language) stream)
  (format stream "~a" (symbol-name obj)))

(defmethod generate-code ((node program-node) (lang basic-language) stream)
  (let* ((generated-code (with-output-to-string (s)
                           (loop for inst in (instructions node)
                                 do (generate-code inst lang s)
                                 do (format s "~%")))))
    ;; let's write s to the desired format
    (format stream "~a" generated-code)))

(gcode add common-lisp ("(+ ~a ~a)") (left-hand rigth-hand))

(gcode mult common-lisp ("(* ~a ~a)") (left-hand rigth-hand))

(gcode assignment common-lisp ("(setf ~a ~a)") (left-hand rigth-hand))

(gcode var-reference common-lisp ("~a") (var-name))

(gcode var-declaration common-lisp
       ("(defparameter ~a nil)")
       (var-name))

(gcode print-node common-lisp
       ("(print ~a)")
       (what-to-print))
