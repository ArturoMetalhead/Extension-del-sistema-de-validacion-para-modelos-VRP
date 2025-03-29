(in-package :lmml)

(defun turn-on-case-sensitivity ()
  "Just make the *readtable* case sensitive"
  (setf (readtable-case *readtable*) :preserve))

(defun turn-off-case-sensitivity ()
  "Just make the *readtable* case sensitive"
  (SETF (READTABLE-CASE *READTABLE*) :UPCASE))

(defun check-case-sensitivity ()
  "Returns the case of the current readtable."
  (READTABLE-CASE *READTABLE*))

(defmacro make-case-sensitive-version-of-function (symbol)
  `(SETF (SYMBOL-FUNCTION ',(INTERN (STRING-DOWNCASE (SYMBOL-NAME symbol))))
          (SYMBOL-FUNCTION ',(INTERN (STRING-UPCASE (SYMBOL-NAME symbol))))))

(defmacro make-case-sensitive-version-of-symbol (symbol)
  `(DEFPARAMETER ,(INTERN (STRING-DOWNCASE (SYMBOL-NAME symbol)))
         ,(INTERN (STRING-UPCASE (SYMBOL-NAME symbol)))))

(defmacro make-case-sensitive-version-of-macro (symbol)
  `(SETF (MACRO-FUNCTION ',(INTERN (STRING-DOWNCASE (SYMBOL-NAME symbol))))
          (MACRO-FUNCTION ',(INTERN (STRING-UPCASE (SYMBOL-NAME symbol))))))

(turn-on-case-sensitivity)

(MAKE-CASE-SENSITIVE-VERSION-OF-MACRO 
      make-case-sensitive-version-of-macro)

(MAKE-CASE-SENSITIVE-VERSION-OF-MACRO 
      make-case-sensitive-version-of-symbol)

(make-case-sensitive-version-of-macro 
      make-case-sensitive-version-of-function)

(make-case-sensitive-version-of-function turn-on-case-sensitivity)

(make-case-sensitive-version-of-function turn-off-case-sensitivity)

(make-case-sensitive-version-of-function check-case-sensitivity)

(make-case-sensitive-version-of-macro defparameter)

(make-case-sensitive-version-of-function list)

(make-case-sensitive-version-of-symbol binary-variable)

(make-case-sensitive-version-of-symbol continuous-variable)

(make-case-sensitive-version-of-symbol integer-variable)

(DEFMACRO variable-declaration (name &KEY
                                       (var-type CONTINUOUS-VARIABLE)
                                       (lower-bound 0)
                                       upper-bound
                                       doc
                                       domain)

  `(VARIABLE-DECLARATION ,name
                         :VAR-TYPE ,var-type
                         :LOWER-BOUND ,lower-bound
                         :UPPER-BOUND ,upper-bound
                         :DOC ,doc
                         :DOMAIN ,domain))

(DEFUN maximize (id func &KEY doc)
  (MAXIMIZE id func :DOC doc))

(DEFUN minimize (id func &KEY doc)
  (MINIMIZE id func :DOC doc))

(make-case-sensitive-version-of-function for-all-quantifier)

(DEFMACRO constraint (id func &KEY quantifiers doc)

  `(CONSTRAINT ,id ,func :QUANTIFIERS ,quantifiers
                         :DOC ,doc))

(make-case-sensitive-version-of-function problem-node)

(make-case-sensitive-version-of-macro problem)

(make-case-sensitive-version-of-symbol +standard-data-reader+)

(DEFMACRO parameter (name &KEY
                            domain
                            doc
                            value
                            (data-reader +standard-data-reader+))

  `(PARAMETER ,name
              :DOMAIN ,domain 
              :DOC ,doc
              :VALUE ,value
              :DATA-READER ,data-reader))

(make-case-sensitive-version-of-function index-at)

(DEFUN range (min-value max-value &KEY (increment 1))

  (RANGE min-value max-value :INCREMENT increment))

(DEFMACRO set (name &KEY
                      dimension
                      value
                      doc
                      (data-reader +standard-data-reader+))

  `(SET ,name
        :DIMENSION ,dimension 
        :VALUE ,value
        :DOC ,doc
        :DATA-READER ,data-reader))

(make-case-sensitive-version-of-function solve)

(make-case-sensitive-version-of-function display)

(make-case-sensitive-version-of-macro sumf)

(make-case-sensitive-version-of-function set-value-of-param)

(make-case-sensitive-version-of-function set-value-of-set)

(make-case-sensitive-version-of-function data-section)

(make-case-sensitive-version-of-function generate-code)

(make-case-sensitive-version-of-symbol gmpl)

(make-case-sensitive-version-of-symbol t)

(make-case-sensitive-version-of-function bformat)

(make-case-sensitive-version-of-function terpri)

(turn-off-case-sensitivity)
