(in-package :lmml)

(defparameter *conjuntos* nil)

(defparameter s.t.-name-counter 0)

(defnode standard-data-reader ()
  ()
  :string-obj ("<std-data-reader>"))

(defparameter +standard-data-reader+ (standard-data-reader))

(defparameter *current-data-reader* +standard-data-reader+)

(defabsnode is-a-reference ()
  ()
  :documentation "Abstract class for references")

(defabsnode has-name ()
  (name)
  :documentation "Base class for classes that have a name.")

(defabsnode has-doc ()
  ((doc :documentation "A description for this element."))
  :documentation "Base class for classes that have a doc.")

(defabsnode has-value ()
  ((value :documentation "The value(s) that this elements should have in the model"))
  :documentation "Base class for classes that can have a value.")

(defabsnode has-current-value ()
  ((current-value :documentation "The current-value(s) that this element should have in the model"))
  :documentation "Base class for classes that can have a current-value.")

(defabsnode has-domain ()
  ((domain :documentation "The domain where this elements should be indexed."))
  :documentation "Base class for classes that can be indexed over a domain.")

(defabsnode has-original-declaration ()
  ((original-declaration :documentation "The original declaration that this class is a reference to."))
  :documentation "Base class for element references.")

(defabsnode has-data-reader ()
  ((data-reader
    :documentation "The data-reader used to read the data."
    :initform (standard-data-reader)))
  :documentation "Base class for classes that require to read data.")

(defabsnode quantifiers ()
  ()
  :documentation "A base class for the quantifiers.")

(defabsnode binary-operation ()
  (left right)
  :documentation "Base class for the binary operations.")

(make-binary-operations ((add "addition" "+" +)
                         (subs "substraction" "-" -)
                         (mult "multiplication" "*" *)
                         (div "division" "/" /)
                         (less-than "less than" "<" <)
                         (less-or-equal "less than or equal to" "<=" <=)
                         (greater-than "greater than" ">" >)
                         (greater-or-equal "greater than or equal to" ">=" >=)
                         (assignment "assign operation" "setq")
                         (equality "equality operation" "=" =)
                         (union "set union" "U")
                         (intersection "set intersection" "^")
                         (difference "set difference" "\\")
                         (cartesian-product "set cartesian product" "x")))

(defparameter list-with-operators-for-tests
  `((add "addition")
    (subs "substraction")
    (mult "multiplication")
    (div "division")
    (less-than "less than")
    (less-or-equal "less than or equal to")
    (greater-than "greater than")
    (greater-or-equal "greater than or equal to")
    (assignment "assign operation")
    (equality "equality operation")
    (union "set union")
    (intersection "set intersection")
    (difference "set difference")
    (cartesian-product "set cartesian product")))

(make-binary-operations
 ((subset    "subset" "subset")
  (not-equal "not-equal" "not-equal")
  (belongs-to "belongs-to" "belongs-to")))

(defparameter list-with-operators-for-tests-2
  `((subset    "subset")
    (not-equal "not-equal")
    (belongs-to "belongs-to")))

(defnode index-at ()
  (var-name indexes)
  :lambda-list (var-name &rest indexes)
  :string-obj ("~a[~{~a~^ ~}]" var-name indexes))

(defnode for-all-quantifier (quantifiers)
  (var-name set-name pred)
  :lambda-list (var-name set-name &optional pred))

(defmethod print-object ((obj for-all-quantifier) stream)
  (format stream "[forall ~a in ~a~a]"
          (var-name obj)
          (set-name obj)
          (if (pred obj) (format nil " where ~a"(pred obj)) "")))

(defun is-a-for-all-quantifier (obj)
  (if (subtypep (class-of obj) 'for-all-quantifier)
      obj))

(defun is-a-for-all-quantifier-list (obj)
  "A for-all-list is a list that start with `for-all-quantifier'"
  (if (and
       (symbolp (first obj))
       (eql (symbol-downcase (first obj))
            (symbol-downcase 'for-all-quantifier)))
      obj))

(defnode var-in-set ()
  (var-name
   set-name)
  :string-obj ("<~a in ~a>" var-name set-name))

(defnode var-from-to ()
  (var-name
   from
   to)
  :string-obj ("<~a from ~a to ~a>" var-name from to))

(defnode sum-node ()
  (sum-bounds
   elements)
  :string-obj ("(sum (~a) ~a)" sum-bounds elements))

(defmacro sumf ((&rest bounds) elements)
  ;; first let's collect all the variable names
  ;; and initialize the var-in-set and var-from-to classes
  (let* (var-refs
         bounds-initializations)

    (loop for bound in bounds
          for symb = (second bound)
          for var-name = (first bound)
          do (progn
               (cond
                 ;; if the second element is the symbol `in'
                 ((eql (symbol-downcase symb) (symbol-downcase 'in))
                  ;; it is a var-in-set clause
                  ;; let's collect the var name
                  (push `(,var-name (var-ref ',var-name)) var-refs)
                  ;; and push the constructor to the initializations 
                  (push `(var-in-set ,var-name ,(third bound))
                        bounds-initializations))
                 ;; if the second element is the symbol `from'
                 ((eql (symbol-downcase symb) (symbol-downcase 'from))
                  ;; it is a var-from-to clause
                  ;; let's collect the var-name
                  (push `(,var-name (var-ref ',var-name)) var-refs)
                  ;; and push the constructor to the initializations 
                  (push `(var-from-to ,var-name
                                      ,(third bound)
                                      ,(fifth bound))
                        bounds-initializations))
                 (t ;; it is neither a var-in-set nor var-from-to
                    ;; it should be a constraint so let's add it
                    ;; without any modifications
                  (push bound
                        bounds-initializations)))))

    `(let* ,(reverse var-refs)
       (make-instance 'sum-node
                      :sum-bounds ,(append `(list)
                                           (reverse bounds-initializations))
                      :elements ,elements))))

(defnode constraint-node (has-doc)
  (id
   func
   quantifiers)
  :lambda-list (id func &key quantifiers doc))

(defmethod print-object ((obj constraint-node) stream)
  (let* ()
    (format stream "[s.t. ~a: ~a~a~a]"
                (id obj)
                (func obj)
                (if (quantifiers obj)
                    (format nil " ~a" (quantifiers obj))
                    "")
                (if (doc obj)
                    (format nil " :doc ~a" (doc obj))
                    ""))))

(defnode continuous-variable-type ()
  ()
  :string-obj ("continuous"))
(defparameter continuous-variable (make-instance 'continuous-variable-type))

(defnode integer-variable-type ()
  ()
  :string-obj ("integer"))
(defparameter integer-variable (make-instance 'integer-variable-type))

(defnode binary-variable-type ()
  ()
  :string-obj ("binary"))
(defparameter binary-variable (make-instance 'binary-variable-type))

(defnode var-ref (is-a-reference
                  has-name
                  has-original-declaration)
  ()
  :lambda-list (name &optional original-declaration)
  :string-obj ("v_~a" name))

(defnode variable-declaration-node (has-name
                                    has-doc
                                    has-domain)
  (value var-type lower-bound upper-bound)
  :lambda-list (name &key
                     (var-type continuous-variable)
                     (lower-bound 0)
                     (value nil)
                     upper-bound
                     doc
                     domain))

(defmethod print-object ((obj variable-declaration-node) stream)
  (let* ()
    (format stream "[var-decl ~a: ~a~a~a~a]"
                (name obj)
                (var-type obj)
                (if (and (numberp (lower-bound obj))
                         (/= (lower-bound obj) 0))
                    (format nil " >= ~a" (lower-bound obj))
                    "")
                (if (numberp (upper-bound obj))
                    (format nil " <= ~a" (upper-bound obj))
                    "")
                (if (domain obj)
                    (format nil " in ~a" (if (listp (domain obj))
                                             (domain obj)
                                             (list (domain obj))))
                    ""))))

(defmacro variable-declaration (name &key
                                       (var-type continuous-variable)
                                       (lower-bound 0)
                                       upper-bound
                                       doc
                                       domain)
  `(let* ((var-decl (make-instance 'variable-declaration-node
                    :name ',name
                    :value nil
                    :var-type ,var-type
                    :lower-bound ,lower-bound
                    :upper-bound ,upper-bound
                    :doc ,doc
                    :domain ,domain)))
     (defparameter ,name
       (var-ref ',name var-decl))
     ;; return var-decl
     var-decl))

(defnode objective-function-node (has-doc)
  (obj id func)
  :lambda-list (obj func &key id doc))

(defclass objective-function-for-minimization (objective-function-node)
  ((obj :initform "minimize")))

(defun minimize (id func &key doc)
  (make-instance 'objective-function-for-minimization
                 :id id
                 :func func
                 :doc doc))

(defmethod print-object ((obj objective-function-for-minimization)
                         stream)
  (format stream "[minimize (~a): ~a~a]"
          (id obj)
          (func obj)
          (if (doc obj)
              (format nil " :doc ~a" (doc obj))
              "")))

(defclass objective-function-for-maximization (objective-function-node)
  ((obj :initform "maximize")))

(defun maximize (id func &key doc)
  (make-instance 'objective-function-for-maximization
                 :id id
                 :func func
                 :doc doc))

(defmethod print-object ((obj objective-function-for-maximization)
                         stream)
  (format stream "[maximize (~a): ~a~a]"
          (id obj)
          (func obj)
          (if (doc obj)
              (format nil " :doc ~a" (doc obj))
              "")))

(defnode instructions-list ()
  (elements)
  :documentation "An list with instructions.")

(defnode problem-definition-node ()
  ((name) (instr)))

(defmethod print-object ((obj problem-definition-node) stream)
  (format stream "problem: ~a~%~a~%"
          (name obj) (instr obj)))

(defun problem-node (name &rest instructions)
  (make-instance 'problem-definition-node
                 :name name
                 :instr (instructions-list instructions)))

(defmacro problem (varname problem-name &rest instructions)
  `(defparameter ,varname
       (make-instance 'problem-definition-node
                     :name ,problem-name
                     :instr (instructions-list (list ,@instructions)))))

(defnode param-ref (is-a-reference
                    has-name
                    has-original-declaration)
  ()
  :lambda-list (name &optional original-declaration)
  :string-obj ("p_~a" name))

(defnode parameter-declaration-node (has-name
                                     has-doc
                                     has-value
                                     has-domain
                                     has-data-reader
                                     has-current-value)
  ()
  :lambda-list (name &key
                     domain
                     value
                     current-value
                     doc
                     (data-reader +standard-data-reader+)))

(defmethod print-object ((obj parameter-declaration-node) stream)
  (let* ()
    (format stream "[param-decl ~a~a~a~a~a~a]"
                (name obj)
                (if (domain obj)
                    (format nil " ~a" (if (listp (domain obj))
                                             (domain obj)
                                             (list (domain obj))))
                    "")
                (if (value obj)
                    (format nil " ~a" (value obj))
                    "")
                (if (doc obj)
                    (format nil ": ~s" (doc obj))
                    "")
                (if (not (eql (type-of (data-reader obj))
                              'standard-data-reader))
                    (format nil ". reader: ~s" (data-reader obj))
                    "")
                (if (current-value obj)
                    (format nil ". current: ~a"
                            (current-value obj))
                    ""))))

(defmacro parameter (name &key
                            domain
                            doc
                            value
                            (data-reader +standard-data-reader+))
  `(let* ((param-declaration
           (make-instance 'parameter-declaration-node
                          :name ',name
                          :value ,value
                          :current-value ,value
                          :doc ,doc
                          :domain ,domain
                          :data-reader ,data-reader)))
     (defparameter ,name
       (param-ref ',name param-declaration))
     ;; return the declaration
     param-declaration))

(defnode range-node ()
  (min-value max-value increment)
  :lambda-list (min-value max-value &key (increment 1))
  :ctr-name range
  :string-obj ("[~a..~a by ~a]" min-value max-value increment))

(defnode set-ref (is-a-reference
                  has-name
                  has-original-declaration)
  ()
  :lambda-list (name &optional original-declaration)
  :string-obj ("S_~a" name))

(defnode set-declaration-node (has-name
                               has-doc
                               has-value
                               has-data-reader
                               has-current-value)
  (dimension)
  :lambda-list (name &key
                     dimension
                     value
                     current-value
                     doc
                     (data-reader +standard-data-reader+)))

(defmethod print-object ((obj set-declaration-node) stream)
  (let* ()
    (format stream "[set-decl ~a~a~a~a~a~a]"
                (name obj)
                (if (dimension obj)
                    (format nil " [~a]" (dimension obj))
                    "")
                (if (value obj)
                    (format nil ", v: ~a" (value obj))
                    "")
                (if (doc obj)
                    (format nil ", ~s" (doc obj))
                    "")
                (if (not (eql (type-of (data-reader obj))
                                    'standard-data-reader))
                          (format nil ". reader: ~s" (data-reader obj))
                          "")
                (if (current-value obj)
                    (format nil ". current: ~a" (current-value obj))
                    ""))))

(defmacro set (name &key
                      dimension
                      value
                      doc
                      (data-reader +standard-data-reader+))
  `(let* ((set-declaration
           (make-instance 'set-declaration-node
                          :name ',name
                          :dimension ,dimension
                          :value ,value
                          :current-value ,value
                          :doc ,doc
                          :data-reader ,data-reader)))
     (defparameter ,name
       (set-ref ',name set-declaration))
     ;; return the set-declaration
     set-declaration))

(defnode display-node ()
  (element)
  :ctr-name display
  :string-obj ("[display ~a]" element))

(defnode solve-node ()
  ()
  :ctr-name solve
  :string-obj ("[solve]"))

(defnode set-value-of-param ()
  (param-name value data-reader)
  :lambda-list (param-name value &optional
                           (data-reader +standard-data-reader+))
  :ctr-body (progn
              (setf (current-value (original-declaration param-name))
                    value
                    (data-reader (original-declaration param-name))
                    data-reader)
              make-ctr))

(defmethod print-object ((node set-value-of-param) stream)
  (format stream "(set-value-of-param ~a to: ~a~a)"
          (param-name node)
          (value node)
          (if (not (eql (type-of (data-reader node))
                        'standard-data-reader))
              (format nil ". reader: ~a" (data-reader node))
              "")
          ))

(defnode set-value-of-set ()
  (set-name value data-reader)
  :lambda-list (set-name value &optional
                           (data-reader +standard-data-reader+))
  :ctr-body (progn
              (setf (current-value (original-declaration set-name))
                    value
                    (data-reader (original-declaration set-name))
                    data-reader)
              make-ctr))

(defmethod print-object ((node set-value-of-set) stream)
  (format stream "(set-value-of-set ~a to: ~a~a)"
          (set-name node)
          (value node)
          (if (not (eql (type-of (data-reader node))
                        'standard-data-reader))
              (format nil ". reader: ~a" (data-reader node))
              "")))

(defnode data-section-node ()
  (instr)
  :lambda-list (&rest instr))

(defun is-a-data-section (obj)
  "If arg is a data section, return it."
  (if (subtypep (class-of obj) 'data-section-node)
      obj))

(defmethod print-object ((node data-section-node) stream)
  (format stream "[~adata section~a~{~a~^, ~}]"
          (if (or
               (and
                (listp (instr node))
                (null (instr node)))
               (and
                (subtypep (class-of (instr node)) 'instructions-list)
                (null (elements (instr node)))))
              "Empty " "")
          (if (or
               (and
                (listp (instr node))
                (null (instr node)))
               (and
                (subtypep (class-of (instr node)) 'instructions-list)
                (null (elements (instr node)))))
              "" ": ")
          (mapcar (lambda (x)
                    (name x))
                  (if (listp (instr node))
                      (instr node)
                      (elements (instr node))))))

(defun data-section (&rest instructions)
  (make-instance 'data-section-node
                 :instr (instructions-list instructions)))

(make-binary-operations-layer-2-names
 ((add +)
  (subs -)
  (mult *)
  (div /)
  (less-than <)
  (less-or-equal <=)
  (greater-than >)
  (greater-or-equal >=)
  (assignment nil)
  (equality =)
  (union nil)
  (intersection nil)
  (difference nil)
  (cartesian-product nil)))

(defparameter list-with-operators-for-tests-layer-2
  `((add "addition" +)
    (subs "substraction" - )
    (mult "multiplication" *)
    (div "division" /)
    (less-than "less than" <)
    (less-or-equal "less than or equal to" <=)
    (greater-than "greater than" >)
    (greater-or-equal "greater than or equal to" >=)
    (assignment "assign operation" nil)
    (equality "equality operation" =)
    (union "set union" nil)
    (intersection "set intersection" nil)
    (difference "set difference" nil)
    (cartesian-product "set cartesian product" nil)))

(make-binary-operations-layer-2-names
 ((subset subset)
  (not-equal !=)
  (belongs-to in)))

(defparameter list-with-operators-for-tests-layer-2
  `((subset "subset" subset)
    (not-equal "not-equal" !=)
    (belongs "belongs-to" in)))

(defmacro constraint (id func &key quantifiers doc)
  (let* ((vars-refs-in-quantifiers
          (loop for q in quantifiers
                when (is-a-for-all-quantifier-list q)
                collect `(,(second q) (var-ref ',(second q))))))
    `(let* ,vars-refs-in-quantifiers
       (constraint-node ,id ,func
                        :quantifiers (list ,@quantifiers)
                        :doc ,doc))))
