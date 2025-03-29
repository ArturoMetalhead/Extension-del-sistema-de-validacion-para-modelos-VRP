(defpackage :gagm
  (:use :cl :cl-user)
  (:export defabsnode
           defnode
           macro
           generate-code
           lang
           node
           gcode
           gcodenil
           gcodenil-exp
           gif
           make-ctr))

(in-package :gagm)

(defun ordered-set-difference (list1 list2)
  "Returns a list with the elements of list1 that do not appear in list2,
but it preserves the order of the elements in list1."
  (loop for elt in list1
        unless (member elt list2) collect elt))

(defun ordered-union (list1 list2)
  "Returns a list with the elements that are either in list1 or in list2,
but it preserves the order of the elements both lists."
  ;; first we add all the elements in list1
  (let* ((union nil))
    (loop for elt in (append list1 list2)
          do (pushnew elt union))
    (reverse union)))

(defun make-keyword (string symbol)
  (read-from-string (format nil string symbol)))

(defun flatten-list (x)
  "Flattens a structure."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun second-elements (list)
  (let* ((count 0)
         (result nil))
    (loop for element in list
          doing(if (eq count 1)
                   (prog2
                       (setf count 0)
                       (setf result (ordered-union
                                     result
                                     (list element))))
                   (setf count 1)))
    result))

(in-package :gagm)

(defun fill-pdict (property-name dict)
  (let ((aux (cdr dict)))
    (prog2
        (setf dict (append (first dict) property-name))
        (setf dict (append (list dict) aux)))))

(defun remove-class-data(class-name dict)
  (loop for  element in dict
        doing (if (eq (car element) class-name)
                  (setf dict (remove element dict))))
  dict)

(defun insert-class-data (dict class-name inherit &rest slots)
  (if (not (eq dict nil))
      (setf dict (remove-class-data class-name dict)))
  (push (list class-name inherit) dict)
  (loop for property-name in slots
        doing (setf dict (fill-pdict property-name dict)))
  dict)

(defun get-data-from-dictionary (class-name dictionary)
  (assoc class-name dictionary))

(defun get-slots-from-class (class-name dictionary)
  (let* ((result (cddr (get-data-from-dictionary
                        class-name dictionary))))
    (mapcan #'(lambda (x) (if (eq x nil) nil (list x))) result)))

(declaim (ftype (function (t t ) t) get-all-slots-from-inherit))

(defun get-all-slots-from-class (class-name dictionary)
  (let* ((result
          (let ((inherit
                 (second (get-data-from-dictionary
                          class-name dictionary))))
            (flatten-list (ordered-union
                           (get-all-slots-from-inherit
                            inherit dictionary)
                           (get-slots-from-class
                            class-name dictionary))))))
    (if (equal result '(nil))
        (setf result nil))
    result))

(defun class-inherit (class-name dictionary)
  (second (get-data-from-dictionary class-name dictionary)))

(defun get-all-slots-from-inherit (inherit dictionary)
  (loop for inherit-act in inherit
        collecting (get-all-slots-from-class inherit-act dictionary)))

(defun get-all-properties (class-name dictionary)
  (second-elements (get-all-slots-from-class
                    class-name dictionary)))

(defparameter slots-dict ())

(defparameter initarg-dict ())

(defparameter accessor-dict ())

(defparameter ctr-func-name-dict ())

(defun clear-properties-dict ()
  (setf slots-dict nil)
  (setf initarg-dict nil)
  (setf accessor-dict nil))

(defmacro create-class-data (class-name
                             documentation
                             inherit
                             slots-def
                             ctr-funtion
                             string-obj)
  `(progn
     ;; definiendo la clase
     (defclass ,class-name
         ,inherit
       ;; esta es la definicion de los slots
          ,slots-def
           (:documentation ,documentation))
     ;; definiendo la funcion constructora.
     ,ctr-funtion
     ;; print-object de la clase definida.
     ;; string-object: formato para definir la cadena a imprimir.
     ;; stream salida del print-object
     (defmethod print-object((node ,class-name) stream)
       (format stream ,(car string-obj)
               ,@(loop for slot in (cdr string-obj)
                       collecting `(,slot node))))))

(in-package :gagm)

(defgeneric generate-code (node language stream)
  (:documentation "This function writes to the stream `stream' the source code of the given node in the specified language."))

(defmacro gcodenil (slot-name)
  "More than a macro this is just an abbreviation."
  `(with-output-to-string (s)
     (generate-code (,slot-name node) lang s)))

(defmacro gcodenil-exp (expresion)
  `(with-output-to-string (s) (generate-code ,expresion lang s)))

(defparameter gcode-slots-options
  '((:optional
     (let* ((result (loop for x in args
                          collect (if (x node)
                                      (gcodenil x)
                                      ""))))
       (format nil "~{~a~%~}" result)))))

(defparameter recognize-patterns
  `((indent (increment-indentation lang))
    (deindent (decrement-indentation lang))))

(defmacro add-new-patterns (symbol pattern)
  `(setf recognize-patterns
         (append recognize-patterns
                 (list (cons ',symbol ,pattern)))))

(defmacro gif (slot-name
               &key
                 (cond-code `(slot-value node ',slot-name))
                 (then-code `(gcodenil ,slot-name))
                 (else-code ""))
  `(if ,cond-code
        ,then-code
        ,else-code))

(defun make-gcodenil-list (list-of-symbols)
  "Given a list of symbols, return a list
where each element is of the form (gcodenil symbol).
      Syntax:
       (make-gcodenil-list list-of-symbols)
       list-of-symbols: is a list of symbols"
  (loop for data in list-of-symbols
        collect (if (eq (type-of data) 'symbol)
                    `(gcodenil ,data)
                    ;; else
                    (if (eq (type-of data) 'cons)
                        (let* ((code (second
                                      (assoc (first data)
                                             gcode-slots-options))))
                          (if (eq code nil)
                              data
                              `(symbol-macrolet
                                   ((args ',(cdr data))) ,code)))))))

(defmacro gformat (stream format-string &rest slots)
  (let* ((gcodenil-list (make-gcodenil-list slots)))
    `(format ,stream ,format-string ,@gcodenil-list)))

(defun make-gformat-instructions (format-strings  args)
`(let* ((result-make-gformat ()))
  ,@(loop for element in format-strings
     collect (if (stringp element)
                 `(setf result-make-gformat
                        (concatenate 'string
                                     result-make-gformat
                                     (gformat nil ,element ,@(pop args))))
                 (if (and (listp element)
                          (eq (first element) 'sformat))
                     `(setf result-make-gformat
                            (concatenate 'string
                                         result-make-gformat
                                         (format nil ,@(cdr element))))
                     element)))
  (format stream  result-make-gformat)))

(defmacro gindformat (stream (&rest format-strings) &rest format-args)
    "The comment and rationale for this macro (in spanish) can be found in the file macro-para-gformat.org"
    (declare (ignorable stream))
    (let* ((gformat-list
            (make-gformat-instructions
             format-strings format-args)))
      `(macrolet ((indent-str (node)
                    (declare (ignore node)) `(make-ind-str lang)))
         (symbol-macrolet ,recognize-patterns
           ,gformat-list))))

(defmacro gcode (class-name language
                 (&rest format)
                 &rest args)
  (if (eq args nil)
      (setf args (list (get-all-properties class-name accessor-dict))))
  `(defmethod generate-code ((node ,class-name) (lang ,language) stream)
     (gindformat stream ,format ,@args)))

(in-package :gagm)

(defun update-dictionary-properties (dict instruction-name inherit args)
  (setf dict (insert-class-data dict instruction-name inherit args))
  dict)

(defparameter *default-print-object-left-key* "(")
(defparameter *default-print-object-right-key* ")")
(defparameter print-object-left-key *default-print-object-left-key*)
(defparameter print-object-right-key *default-print-object-right-key*)

(defun update-left-key (new-key)
  "Sets the new left key for the print-object."
  (setf print-object-left-key new-key))

(defun update-right-key (new-key)
  "Sets the new rigth key for the print-object."
  (setf print-object-right-key new-key))

(defun update-key (l-key r-key)
  "Sets the new left and right keys for the print-object."
  (update-left-key l-key)
  (update-right-key r-key))

(defun set-print-object-keys-to-default ()
       "Sets the new left and right keys for the print-object."
       (update-left-key *default-print-object-left-key*)
       (update-right-key *default-print-object-right-key*))

(defun standard-print-object (name args separator function)
  (let* ((format-string (format nil "~a~a ~~{~~a~a~~}~a"
                                print-object-left-key
                                name separator
                                print-object-right-key)))
    (format nil format-string
            (loop for arg in args
                  collecting  (list arg function)))))

(defun chek-parameters-func (parameters code)
  (let* ((result ()))
    (loop for element in code
          doing
          (if
           (member element parameters)
           (setf result
                 (append result
                         (list (read-from-string
                                (format nil "'~a" element)))))
           (if
            (atom element)
            (setf result (append result (list element)))
            (setf result (append result
                                 (list
                                  (chek-parameters-func
                                   parameters element)))))))
    result))

(defun remove-type-to-type-parameters (parameters)
  (loop for (type name) in parameters
        collect  name))

(defun normalize-code (parameters code)
  (if (not (atom (car parameters)))
      (setf parameters (remove-type-to-type-parameters parameters)))
  (chek-parameters-func parameters code))

(defmacro g-def-void-func (funtion-name)
  `(defmacro ,funtion-name (name parameters code)
     (setf code-aux (normalize-code parameters code))
     `(method-class ',name ',parameters ,code-aux)))

(defun only-first-element-in-list (args_trio)
  (loop for x in args_trio
        collect (if (atom x)
                    x
                    (car x))))

(defmacro defnode (class-name inherit
                   (&rest args)
                   &key
                     (documentation "No any documentation for this node.")
                     (ctr-type 'function)
                     (ctr-name class-name)
                     (lambda-list nil lambda-list-p)
                     (lambda-key nil lambda-key-p)
                      ;; para indicar donde se debe llamar
                      ;; al make-instance, se pone la palabra make-ctr
                     (ctr-body 'make-ctr)
                     ;; before de la funcion constructora
                     ;; (before nil before-p)
                     (string-obj nil string-obj-p))


  (let* ((filter-args (only-first-element-in-list args))
         (slots-inherit (get-all-slots-from-inherit inherit slots-dict))
         (filter-args (ordered-set-difference filter-args slots-inherit))
         (slots-def
          (loop for data in args
                collect (if (atom data)
                            (list data
                                  :accessor data
                                  :initarg (make-keyword ":~a" data)
                                  :allocation :instance)
                            (let* ((name (first data)))
                              (destructuring-bind
                                    (slot-name
                                     &key
                                     (accessor name)
                                     (initarg (make-keyword ":~a" name))
                                     (documentation "empty doc")
                                     (initform nil)
                                     (allocation :instance)
                                     ;; optional es una lista para
                                     ;; modificadores que se quieran
                                     ;; agregar.
                                     ;; por ejemplo en la
                                     ;; definicion del slot x:
                                     ;; (x :accesor superx
                                     ;;    :optional (:read 34))
                                     (optional nil)) 
                                  data
                                (append (list slot-name
                                              :accessor accessor
                                              :initarg initarg
                                              :allocation allocation
                                              :initform initform
                                              :documentation documentation)
                                        optional)))))))
    ;; recordar poner que sea cualquier modificador,
    ;; es solo pasarselo por parametor y
    ;; append a la lista de slots-def
    (let* ((accessor-def (loop for data in slots-def
                            collecting (list (car data) (third data))))
           (initarg-def (loop for data in slots-def
                           collecting  (list (car data)
                                             (make-keyword
                                              "~a"
                                              (fifth data))))))
      (setf slots-dict (update-dictionary-properties
                        slots-dict
                        class-name
                        inherit
                        filter-args))
      (setf accessor-dict (update-dictionary-properties
                           accessor-dict
                           class-name
                           inherit
                           accessor-def))
      (setf initarg-dict (update-dictionary-properties
                          initarg-dict
                          class-name
                          inherit
                          initarg-def)))
    (let* ((all-accessor (get-all-properties
                          class-name accessor-dict))
           (all-initarg (get-all-properties
                         class-name initarg-dict))
           ;; El comentario siguiente es
           ;; valido solo para las variables que
           ;; se definen a continuacion.         
           ;; Esta region del let* es solo
           ;; para asignar valores por defecto
           ;; a los parametros con el modificador &key activo.
           (ctr-type (if (eq ctr-type 'abstract)
                         'abstract
                         (if (eq ctr-type 'macro)
                             'defmacro
                             (if (eq ctr-type 'function)
                                 'defun
                                 'defmethod))))
           (parameters (if (not lambda-list-p)
                           (if (not lambda-key-p)
                               all-initarg
                               (append (list lambda-key) all-initarg))
                                  lambda-list))
           (new-ctr-body (let* ((body-aux
                                 (if (eq ctr-type 'defun)
                                     `(make-instance
                                       ',class-name
                                       ,@(loop for initarg-name
                                               in all-initarg
                                               collect (make-keyword
                                                        ":~a" initarg-name)
                                               collect (make-keyword
                                                        "~a" initarg-name)))
                                     (if  (eq ctr-type 'defmacro)
                                          `(make-instance
                                            ',class-name
                                            ,@(loop for initarg-name
                                                    in all-initarg
                                                    collect
                                                    (make-keyword
                                                     ":~a" initarg-name)
                                                    collect
                                                    (read-from-string
                                                     (concatenate
                                                      'string
                                                      "`,"
                                                      (symbol-name
                                                       initarg-name)))))))))
                           `(symbol-macrolet ((make-ctr ,body-aux))
                              ,ctr-body)))
           (n-string-obj (if (and string-obj-p
                                  (not (eq nil string-obj)))
                             string-obj
                             (append (list (standard-print-object
                                            class-name all-accessor
                                            " "  "~a"))
                                     all-accessor)))
           (ctr-func (if (eq ctr-type 'abstract)
                         nil
                         `(,ctr-type
                           ,ctr-name
                           ,parameters
                           ,new-ctr-body))))
      (setf ctr-func-name-dict
            (append (list (list class-name ctr-name))
                    (remove-class-data class-name ctr-func-name-dict)))
      `(create-class-data ,class-name
                          ,documentation
                          ,inherit
                          ,slots-def
                          ,ctr-func
                          ,n-string-obj))))

(defmacro defabsnode (class-name inherit (&rest args)
                      &key
                        (documentation
                         "No any documentation for this node.")
                        (ctr-type 'abstract)
                        (string-obj nil))
  `(defnode ,class-name ,inherit
     ,args
     :documentation ,documentation
     :ctr-type ,ctr-type
     :string-obj ,string-obj))
