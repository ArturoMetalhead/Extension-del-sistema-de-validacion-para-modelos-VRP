(in-package :lmml)

(defun make-slots-for-basic-language (slot-list)
  (loop for (prefix symbol name) in slot-list
        for node-name = (symb prefix "-symbol")
        for initarg = (make-keyword node-name)
        for docstring = (format nil "The symbol used for the ~a." name)
        collecting `(,node-name
                      :accessor ,node-name :initarg ,initarg
                      :initform ,symbol
                      :documentation ,docstring)))

(defmacro define-basic-language (language-name
                                 super-classes
                                 operators-list)
  "Creates the class named language-name, with parents super-classes and adds all the operators in the list.
 Syntax:
   (define-basic-language language-name super-classes operators-list)
  operators-list is a list where each element is of the form:
     (node-prefix symbol-used operation-name).
   Example:
     (define-basic-language basic-language () 
                             ((plus \"+\" \"addition\")
                              (minus \"-\" \"substraction\")))
    expands into
      (defclass basic-language ()
        ((plus-symbol :accessor plus-symbol :initarg :plus-symbol
                      :initform \"+\"
                      :documentation \"The symbol used for the addition.\")
         (minus-symbol :accessor minus-symbol :initarg :minus-symbol
                       :initform \"-\"
                       :documentation \"The symbol used for the addition.\"))
        (:documentation \"The basic class for languages with arithmetic operators.\"))"
  `(defclass ,language-name ,super-classes
     ,(make-slots-for-basic-language (if (symbolp operators-list)
                                         (symbol-value operators-list)
                                         operators-list))
     (:documentation "The basic class for languages with arithmetic operators.")))

(defparameter *basic-language-binary-operators*
  '((add                "+" "addition")
    (subs               "-" "substraction")
    (mult               "*" "multiplication")
    (div                "/" "division")
    (less-than          "<" "less than comparison")
    (less-or-equal      "<=" "less than or equal to")
    (greater-than       ">" "greater than comparison")
    (greater-or-equal   ">=" "greater than or equal to")
    (equality           "=" "equality comparison")
    (assignment         nil "assignment operation")
    (union              nil "union of sets")
    (intersection       nil "intersection of sets")
    (difference         nil "set difference")
    (cartesian-product  nil "cartesian product X times Y"))
  "A list with the operators used by the basic language.")

(define-basic-language basic-language nil 
  *basic-language-binary-operators*)

(defmethod generate-code ((node number)
                          (lang basic-language)
                          stream)
  (format stream "~a" node))

(defmethod generate-code ((node string)
                          (lang basic-language)
                          stream)
  (format stream "~a" node))

(defmethod generate-code((node list)
                         (lang basic-language)
                         (stream t))
  (format stream "~{~a~^ ~}"
          (mapcar (lambda (x) (gcodenil-exp x))
                  node)))

(defmethod generate-code((node instructions-list)
                         (lang t)
                         (stream t))
  (format stream "~{~a~^~2%~}"
          (mapcar (lambda (x) (gcodenil-exp x))
                  (elements node))))

(defnode infix-language (basic-language)
  ())

(defparameter *basic-language-binary-operators-except-mult-div-subs*
  '((add                "+" "addition")
    (less-than          "<" "less than comparison")
    (less-or-equal      "<=" "less than or equal to")
    (greater-than       ">" "greater than comparison")
    (greater-or-equal   ">=" "greater than or equal to")
    (equality           "=" "equality comparison")
    (assignment         nil "assignment operation")
    (union              nil "union of sets")
    (intersection       nil "intersection of sets")
    (difference         nil "set difference")
    (cartesian-product  nil "cartesian product X times Y"))
  "A list with the operators used by the basic language.")

(generate-code-for-binary-operators
 infix-language
 "~a ~a ~a"
 (left op right)
 *basic-language-binary-operators-except-mult-div-subs*)

(defmethod generate-code ((node mult-op)
                          (lang infix-language)
                          stream)
  (let* ((format-string (concatenate 'string
                                     (if (or
                                          (subtypep (type-of (left node)) 'add-op)
                                          (subtypep (type-of (left node)) 'subs-op))
                                         "(~a)"
                                         "~a")
                                     "~a"
                                     (if (or
                                          (subtypep (type-of (right node)) 'add-op)
                                          (subtypep (type-of (right node)) 'subs-op))
                                         "(~a)"
                                         "~a"))))
    (format stream format-string
            (gcodenil left)
            (gcodenil-exp (mult-symbol lang))
            (gcodenil right))))

(defmethod generate-code ((node div-op)
                          (lang infix-language)
                          stream)
  (let* ((format-string (concatenate 'string
                                     (if (or
                                          (subtypep (type-of (left node)) 'add-op)
                                          (subtypep (type-of (left node)) 'subs-op))
                                         "(~a)"
                                         "~a")
                                     "~a"
                                     (if (or
                                          (subtypep (type-of (right node)) 'add-op)
                                          (subtypep (type-of (right node)) 'subs-op))
                                         "(~a)"
                                         "~a"))))
    (format stream format-string
            (gcodenil left)
            (gcodenil-exp (div-symbol lang))
            (gcodenil right))))

(defmethod generate-code ((node subs-op)
                          (lang infix-language)
                          stream)
  (let* ((format-string (concatenate 'string
                                     "~a ~a "
                                     (if (or
                                          (subtypep
                                           (type-of (right node)) 'add-op)
                                          (subtypep
                                           (type-of (right node)) 'subs-op))
                                         "(~a)"
                                         "~a"))))
    ;; (format t "format string: ~a~%" format-string)

    (format stream format-string
            (gcodenil left)
            (gcodenil-exp (subs-symbol lang))
            (gcodenil right))))

(defnode symbol-separated-language ()
  ((separator :accessor separator
              :initarg :separator
              :initform ";"
              :documentation "The separator for the language")))

(defmacro node-ends-with-separator (class-name language)
  `(defmethod generate-code :after ((node ,class-name)
                                    (lang ,language)
                                    stream)
              (format stream "~a" (separator lang))))

(defnode camel-case-language ()
  ()
  :documentation "A language with camelCase notation.")

(defun symbol-to-camelcase (symbol)
  "Returns a string with the symbol-name of symbol in camelcase notation."
  (let* ((symbol-string (string-downcase (symbol-name symbol)))
         (camel-case-string "")
         (up-case nil))
    (if (> (length symbol-string) 1)
        (loop for c across symbol-string
              when (not (char-equal c #\-))
              do (setf camel-case-string
                       (concatenate 'string camel-case-string
                                    (if up-case (string-upcase c)
                                        (string c))))
              do (setf up-case (char-equal c #\-)))
        (setf camel-case-string symbol-string))
    ;; return camel-case-string
    camel-case-string))

(defmethod generate-code ((node symbol)
                          (language camel-case-language)
                          stream)
  (format stream "~a" (symbol-to-camelcase node)))

(defnode underscore-language ()
  ()
  :documentation "A language with underscore_notation.")

(defmethod convert-to-underscore ((obj symbol))
  "Returns a string with the symbol-name of symbol in underscore_notation."
  (let* ((current-string (symbol-name obj))
         (underscored-string ""))
    (loop for c across current-string
          do (setf underscored-string
                   (concatenate 'string
                                underscored-string
                                (if (char-equal c #\-) "_"
                                    (string c)))))
  ;; return underscored-string
  underscored-string))

(defmethod convert-to-underscore ((string string))
  "If argument is a string, just replace dashes with underscores."
  (let* ((current-string string)
         (underscored-string ""))
    (loop for c across current-string
          do (setf underscored-string
                   (concatenate 'string
                                underscored-string
                                (if (char-equal c #\-) "_"
                                    (string c)))))
  ;; return underscored-string
  underscored-string))

(defmethod generate-code ((node t)
                          (language underscore-language)
                          stream)
  (format stream "~a" (convert-to-underscore node)))

(defmethod write-name ((node t)
                       (lang t)
                       stream)
  (format stream "~a"
          (convert-to-underscore 
           (name node))))

(defnode variable-lowercase-language ()
  ()
  :documentation "A language with variable-lowercase convention.")

(defmethod write-name ((node var-ref)
                       (lang variable-lowercase-language)
                       stream)
  (format stream "~a"
          (convert-to-underscore 
           (string-downcase (name node)))))

(defmethod write-name ((node variable-declaration-node)
                       (lang variable-lowercase-language)
                       stream)
  (format stream "~a"
          (convert-to-underscore 
           (string-downcase (name node)))))

(defmethod generate-code ((node var-ref)
                          (lang variable-lowercase-language)
                          stream)
  (format stream "~a"
          (with-output-to-string (s)
            (write-name node lang s))))

(defnode set-uppercase-language ()
  ()
  :documentation "A language with set-uppercase convention.")

(defmethod write-name ((node set-ref)
                       (language set-uppercase-language)
                       stream)
  (format stream "~a"
          (convert-to-underscore 
           (string-upcase (name node)))))

(defmethod write-name ((node set-declaration-node)
                       (lang set-uppercase-language)
                       stream)
  (format stream "~a"
          (convert-to-underscore 
           (string-upcase (name node)))))

(defmethod generate-code ((node set-ref)
                          (lang set-uppercase-language)
                          stream)
  (format stream "~a"
          (with-output-to-string (s)
            (write-name node lang s))))
