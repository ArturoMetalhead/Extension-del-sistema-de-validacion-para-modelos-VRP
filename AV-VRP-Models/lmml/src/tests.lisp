(let* ()
  (bformat t "Testing symbol-downcase")
  (format t "~a~%" (symbol-downcase 'hello))
  (format t "~a~%" (symbol-downcase 'HELLO))
  (format t "~a~%" (symbol-downcase 'hELLo)))

(progn
  (bformat t "Testing length with lists")
  (format t "Expect 3: ~a~%" (length '(1 2 3)))
  (format t "Expect 2: ~a~%" (length '((1 2) 3))))

(progn
  (bformat t "Testing length with strings")
  (format t "Expect 5: ~a~%" (length "Hello"))
  (format t "Expect 0: ~a~%" (length ""))
  (format t "Expect 2: ~a~%" (length "hh")))

(progn
  (bformat t "Testing length with strings")
  (format t "Expect 5: ~a~%" (length 'Hello))
  (format t "Expect 3: ~a~%" (length 'one))
  (format t "Expect 11: ~a~%" (length 'hello-world)))

(progn
  (bformat t "Testing length with strings")
  (format t "Expect 5: ~a~%" (length 'Hello))
  (format t "Expect 0: ~a~%" (length nil))
  (format t "Expect 0: ~a~%" (length nil)))

(progn
  (bformat t "Testing length with numbers")
  (format t "Expect 5: ~a~%" (length 12345))
  (format t "Expect 1: ~a~%" (length 1))
  (format t "Expect 2: ~a~%" (length -1))
  (format t "Expect 8: ~a~%" (length -1.35e14)))

(make-binary-operations ((add "addition")
                       (subs "substraction")))

(make-binary-operations-layer-2-names ((add +)
                                       (another nil)
                                       (subs -)))

(generate-code-for-binary-operators infix-language
                                    "(~a) ~a (~a)"
                                    (left op right)
                                    ((add +)
                                     (another nil)
                                     (subs -)))

(generate-value-for-binary-operators
 ((add  cl:+)
  (subs cl:-)))

(let* ((param-decl (parameter-declaration-node
                    "p1" :value 16))
       (p1 (param-ref "p1" param-decl))
       (node1 (add-op 1 3))
       (node2 (subs-op 1 3))
       (node3 (add-op p1 3)))

  (bformat t "Testing value for binary-ops")
  (format t "Value of +: ~2d~%" (value node1))
  (format t "Value of -: ~2d~%" (value node2))
  (format t "with param-ref: ~2d~%" (value node3))
  )

(generate-value-for-binary-operators
 ((add  cl:+)
  (subs cl:-)))

(let* ((param-decl (parameter-declaration-node
                    "p1" :value 16))
       (p1 (param-ref "p1" param-decl))
       (node1 (add-op 1 3))
       (node2 (subs-op 1 3))
       (node3 (add-op p1 3)))

  (bformat t "Testing value for binary-ops")
  (format t "Value of +: ~2d~%" (value node1))
  (format t "Value of -: ~2d~%" (value node2))
  (format t "with param-ref: ~2d~%" (value node3))
  )

(let* ((l1 {1 2 3})
       (l2 {1 (cl:+ 1 1) 3}))
  (bformat t "Testing reader macro for {}")
  (format t "l1: ~a~%" l1)
  (format t "l2: ~a~%" l2))

(let* ()
  (bformat t "Testing reader macro for []")
  (format t "(index-at 1): ~a~%" `[1])
  (format t "(index-at 1 2): ~a~%" `[1 2])
  (format t "(index-at 1 2): ~a~%" `[1 (cl:+ 1 1)]))

(let* ((node (standard-data-reader)))
  (print node))

(progn
  (bformat t "Testing binary-operators:")
  (loop for (op description) in list-with-operators-for-tests
        for node = (make-instance (symb op '-op) :left 1 :right 2)
        do (format t "Operation ~a:~38t ~a~%" description node)))

(progn
  (bformat t "Testing new binary-operators (2022-08-29):")
  (loop for (op description) in list-with-operators-for-tests-2
        for node = (make-instance (symb op '-op) :left 1 :right 2)
        do (format t "Operation ~a:~38t ~a~%" description node)))

(let* ((node (index-at 'x 1 2 3 4)))
  (print node))

(let* ((node (for-all-quantifier 'i 'J nil)))
  node)

(let* ((node (for-all-quantifier 'i 'J (greater-than-op 'i 3))))
  (format t "forall with predicate:~% ~a~%" node))

(let* ((node (for-all-quantifier 'i 'J (greater-than-op 'i 3))))
  (bformat t "Testing is-a-for-all-quantifier")
  (format t "Expect non nil: ~a~%" (is-a-for-all-quantifier node))
  (format t "Expect nil:     ~a~%" (is-a-for-all-quantifier 5)))

(let* ((node '(for-all-quantifier i J)))
  (bformat t "Testing is-a-for-all-quantifier-list")
  (format t "Expect non nil: ~a~%" (is-a-for-all-quantifier-list node))
  (format t "Expect nil:     ~a~%" (is-a-for-all-quantifier 5))
  (format t "Expect nil:     ~a~%" (is-a-for-all-quantifier-list
                                    '(for-all i in J))))

(let* ((node (var-in-set 'i 'K))
       (s1-decl (set-declaration-node "S1"))
       (s1 (set-ref "S1" s1-decl))
       (node2 (var-in-set 'i s1))
       )
  (bformat t "Testing var in set")
  (format t "i in K: ~a~%" node)
  (format t "i in S1: ~a~%" node2))

(let* ((node (var-from-to 'i 1 5))
       (node2 (var-from-to 'i 10 15)))
  (bformat t "Testing var from to")
  (format t "i from 1 to 5:   ~a~%" node)
  (format t "i from 10 to 15: ~a~%" node2))

(let* ((node1 (sum-node (list (var-in-set 'i 'K))  (add-op 'i 2)))
       (node2 (sum-node (list (var-in-set 'i 'K)
                              (var-in-set 'j 'K))
                        (add-op 'i 'j)))
       (node3 (sum-node (list (var-from-to 'i 10 15))
                        (add-op 'i 5)))
       (node4 (sum-node (list (var-from-to 'i 10 15)
                              (var-in-set 'j 'K))
                        (add-op 'i 'j)))
       (node5 (sum-node (list (var-in-set 'i 'K)
                              (< 'i 8))
                        (add-op 'i 1)))
       )
  (bformat t "Testing sum-node layer 1")
  (format t "var-in-set:  ~a~%" node1)
  (format t "2 sum-bounds ~a~%" node2)
  (format t "var-from-to  ~a~%" node3)
  (format t "2 types      ~a~%" node4)
  (format t "constraint   ~a~%" node5)
  )

(let* ((j-decl (set-declaration-node "J"))
       (J (set-ref "J" j-decl))
       (node1 (sumf ((i in J)) (add-op i 2)))
       (node2 (sumf ((i in J) (k in J) ) (+ i k)))
       (node3 (sumf ((i in J) (< i 5) ) (+ i 2)))
       (node4 (sumf ((i from 1 to 5)) (add-op i 2)))
       (node5 (sumf ((i from 1 to 5)
                     (j from 1 to 10) )
                    (+ i j)))
       (node6 (sumf ((i from 1 to 10) (< i 5) ) (+ i 2)))
       )
  (bformat t "Testing var-in-set (layer 2)")
  (format t "One var:         ~a~%" node1)
  (format t "Two vars:        ~a~%" node2)
  (format t "A constraint:    ~a~%" node3)
  (terpri)
  (bformat t "Testing var-from-to (layer 2)")
  (format t "One var from     ~a~%" node4)
  (format t "Mixed bounds:    ~a~%" node5)
  (format t "Mixed and const: ~a~%" node6)
  )

(let* ((node1 (constraint-node 1 (less-than-op 'x 5)))
       (node2 (constraint-node 2 (less-than-op 'x 5)
                               :quantifiers
                               (for-all-quantifier 'x 'Y nil)))
       (node3 (constraint-node 3 (less-than-op 'x 'z)
                               :quantifiers
                               (list
                                (for-all-quantifier 'x 'Y nil)
                                (for-all-quantifier 'z 'Y nil))))
       (node4 (constraint-node 4 (less-than-op 'x 'z)
                               :quantifiers
                               (for-all-quantifier 'x 'Y nil)
                               :doc "A constraint"))
       (node5 (constraint-node 4 (less-than-op 'x 'z)
                               :quantifiers
                               (list
                                (for-all-quantifier 'x 'Y nil)
                                (< 'x 3))))
       )
  (bformat t "Testing constraint node")
  (format t "simple constranint: ~A~%" node1)
  (format t "constranint with:   ~A~%" node2)
  (format t "more quantifiers:   ~A~%" node3)
  (format t "documentation:      ~A~%" node4)
  (format t "additional cond:    ~A~%" node5))

(let* ()
  (bformat t "Testing continuous-variable")
  (format t "~a~%" continuous-variable))

(let* ()
  (bformat t "Testing integer-variable")
  (format t "~a~%" integer-variable))

(let* ()
  (bformat t "Testing binary-variable")
  (format t "~a~%" binary-variable))

(let* ((node1 (var-ref 'i))
       (node2 (var-ref 'i 'var-decl)))
  (bformat t "testing var ref")
  (format t "var-ref: ~a~%" node1)
  (format t "var-ref with var-decl: ~a~%" node2)
  (format t "var-decl: ~a~%" (original-declaration node2)))

(let* ((node1 (variable-declaration-node 'x))
       (node2 (variable-declaration-node 'x :var-type binary-variable))
       (node3 (variable-declaration-node 'x :var-type integer-variable
                                         :lower-bound 1))
       (node4 (variable-declaration-node 'x :var-type binary-variable
                                         :lower-bound 1
                                         :upper-bound 10))
       (node5 (variable-declaration-node 'x :var-type integer-variable
                                         :lower-bound 1
                                         :upper-bound 10
                                         :domain 'H))
       (node6 (variable-declaration-node 'x :var-type continuous-variable
                                         :lower-bound 1
                                         :upper-bound 10
                                         :domain `(H H)))
       (node7 (variable-declaration-node 'x
                                         :var-type binary-variable
                                         :doc "A binary variable"))
       )
  (bformat t "Testing variable-declaration node")
  (format t "var decl:~20t ~A~%" node1)
  (format t "var decl with type:~20t ~A~%" node2)
  (format t "with lower-bound:~20t ~A~%" node3)
  (format t "with upper-bound:~20t ~A~%" node4)
  (format t "with domain:~20t ~A~%" node5)
  (format t "with domains:~20t ~A~%" node6)
  (format t "with doc:~20t ~A~%" (doc node7)))

(let* ((node1 (variable-declaration x1))
       (node2 (variable-declaration x2
                                    :var-type binary-variable))
       (node3 (variable-declaration x3
                                    :var-type integer-variable
                                    :lower-bound 1))
       (node4 (variable-declaration x4
                                    :var-type binary-variable
                                    :lower-bound 1
                                    :upper-bound 10))
       (node5 (variable-declaration x5
                                    :var-type integer-variable
                                    :lower-bound 1
                                    :upper-bound 10
                                    :domain 'H))
       (node6 (variable-declaration x6
                                    :var-type continuous-variable
                                    :lower-bound 1
                                    :upper-bound 10
                                    :domain `(H H)))
       (node7 (variable-declaration x7
                                    :var-type binary-variable
                                    :doc "A binary variable."))
       )
  (bformat t "Testing variable-declaration node")
  (format t "var decl:~20t ~A~%" node1)
  (format t "var decl with type:~20t ~A~%" node2)
  (format t "with lower-bound:~20t ~A~%" node3)
  (format t "with upper-bound:~20t ~A~%" node4)
  (format t "with domain:~20t ~A~%" node5)
  (format t "with domains:~20t ~A~%" node6)
  (format t "with doc:~20t ~A~%" (doc node7))

  (bformat t "Now testing the references")
  (format t "~a: ~a~%" x1 (name x1))
  (format t "~a: ~a~%" x2 (name x2))
  (format t "~a: ~a~%" x3 (name x3))
  (format t "~a: ~a~%" x4 (name x4))
  (format t "~a: ~a~%" x5 (name x5))
  (format t "~a: ~a~%" x6 (name x6))
  (format t "~a: ~a~%" x7 (name x7))

  (bformat t "Now testing the original-var in the references")
  (format t "~a: ~a~%" x1 (original-declaration x1))
  (format t "~a: ~a~%" x2 (original-declaration x2))
  (format t "~a: ~a~%" x3 (original-declaration x3))
  (format t "~a: ~a~%" x4 (original-declaration x4))
  (format t "~a: ~a~%" x5 (original-declaration x5))
  (format t "~a: ~a~%" x6 (original-declaration x6))
  (format t "~a: ~a~%" x7 (original-declaration x7))
  )

(let* ((node1 (objective-function-node 'max 'function))
       (node2 (objective-function-node 'min 'function :id 1))
       (node3 (objective-function-node 'min 'function :id 1
                                       :doc "The obj function")))
  (bformat t "Testing obj-function")
  (format t "just basic:~% ~a~%" node1)
  (format t "with id:~% ~a~%" node2)
  (format t "with doc:~% ~a~%" node3))

(let* ((node (objective-function-node 1 (add-op (mult-op 2 'x)
                                           (mult-op 4 'y)))))
  (print node))

(let* ((node1 (minimize 1 'function))
       (node2 (minimize 2 'function))
       (node3 (minimize 3 'function
                        :doc "An obj function to minimize")))
  (bformat t "Testing obj-function")
  (format t "just basic:~% ~a~%" node1)
  (format t "with id:~% ~a~%" node2)
  (format t "with doc:~% ~a~%" node3))

(let* ((node1 (minimize 
               1 (add-op (mult-op 2 'x)
                         (mult-op 4 'y))))
       (node2 (minimize 
               1 (add-op (mult-op 2 'x)
                         (mult-op 4 'y))
               :doc "A basic function.")))
  (bformat t "Testing minimize with a function" )
  (format t "with-function:~30t ~a~%" node1)
  (format t "with-function and doc:~30t ~a~%" node2))

(let* ((node1 (maximize 1 'function))
       (node2 (maximize 2 'function))
       (node3 (maximize 3 'function
                        :doc "An obj function to maximize")))
  (bformat t "Testing maximize")
  (format t "just basic:~% ~a~%" node1)
  (format t "with id:~% ~a~%" node2)
  (format t "with doc:~% ~a~%" node3))

(let* ((node1 (maximize 
               1 (add-op (mult-op 2 'x)
                         (mult-op 4 'y))))
       (node2 (maximize 
               1 (add-op (mult-op 2 'x)
                         (mult-op 4 'y))
               :doc "A basic function.")))
  (bformat t "Testing maximize with a function" )
  (format t "with-function:~30t ~a~%" node1)
  (format t "with-function and doc:~30t ~a~%" node2))

(print (instructions-list (list 1 2 3 4)))

(let* ((p1 (problem-definition-node "example 1" (list 1 2 3 4))))
  (format t "p1: ~a~%" p1))

(let* ((p1 (problem-node "example 1"  1 2 3 4)))
  (format t "p1: ~a~%" p1))

(let* ()
  (problem p1 "example problem 1"
           1 2 3 4)
  (format t "p1: ~a~%" p1))

(let* ((node1 (param-ref  'i))
       (node2 (param-ref  'i 'original-declaration)))
  (bformat t "testing param ref")
  (format t "param-ref: ~a~%" node1)
  (format t "param-ref: ~a~%" node2)
  (format t "original-declaration: ~a~%" (original-declaration node2)))

(let* ((node1 (parameter-declaration-node 'p))
       (node2 (parameter-declaration-node 'P :domain 'I))
       (node3 (parameter-declaration-node 'P :domain '(I J)))
       (node4 (parameter-declaration-node 'P :value 5))
       (node5 (parameter-declaration-node 'P :value 5
                                          :doc "A documentation"))
       (node6 (parameter-declaration-node 'P :value 5
                                          :data-reader 8))
       (node7 (parameter-declaration-node 'P :value 5
                                          :data-reader 8
                                          :current-value 15))
       (tab-pos 25))

  (bformat t "Testing parameter-declaration node")
  (format t "param decl:~vt ~A~%" tab-pos node1)
  (format t "param decl with domain:~vt ~A~%" tab-pos node2)
  (format t "with two indexes:~vt ~A~%" tab-pos node3)
  (format t "with value:~vt ~A~%" tab-pos node4)
  (format t "with value and doc:~vt ~A~%" tab-pos node5)
  (format t "with value and doc:~vt ~A~%" tab-pos node6)
  (format t "with value and doc:~vt ~A~%" tab-pos node7))

(let* ((node1 (parameter P1))
       (node2 (parameter P2 :domain 'I))
       (node3 (parameter P3 :domain '(I J)))
       (node4 (parameter P4 :value 5))
       (node5 (parameter P5 :value 5
                         :doc "A documentation"))
       (node6 (parameter P6 :value 5
                         :data-reader 8))
       )
  (bformat t "Testing parameter-declaration macro")
  (format t "param decl:~40t ~A~%" node1)
  (format t "param decl with domain:~40t ~A~%" node2)
  (format t "with two indexes:~40t ~A~%" node3)
  (format t "with value:~40t ~A~%" node4)
  (format t "with documentation:~40t ~A~%" node5)
  (format t "with data-reader:~40t ~A~%" node6)

  (bformat t "Now testing the param-refs")
  (format t "P1: ~a~%" p1)
  (format t "P2: ~a~%" p2)
  (format t "P3: ~a~%" p3)
  (format t "P4: ~a~%" p4)
  (format t "P5: ~a~%" p5)
  (format t "P6: ~a~%" p6)

  (bformat t "Testing the original-declaration")
  (format t "P1: ~a~%" (original-declaration p1))
  (format t "P2: ~a~%" (original-declaration p2))
  (format t "P3: ~a~%" (original-declaration p3))
  (format t "P4: ~a~%" (original-declaration p4))
  (format t "P5: ~a~%" (original-declaration p5))
  (format t "P6: ~a~%" (original-declaration p6))
  )

(let* ((node1 (range 1 5))
       (node2 (range 1 5 :increment 2)))
  (bformat t "Testing range-node")
  (format t "range: ~a~%" node1)
  (format t "range: ~a~%" node2))

(let* ((node1 (set-ref  'I))
       (node2 (set-ref  'I 'original)))
  (bformat t "testing set ref")
  (format t "set-ref: ~a~%" node1)
  (format t "set-ref: ~a~%" node2)
  (format t "original: ~a~%" (original-declaration node2)))

(let* ((node1 (set-declaration-node 'S))
       (node2 (set-declaration-node 'S :dimension 2))
       (node3 (set-declaration-node 'S :value '(1 2 3)))
       (node4 (set-declaration-node 'S :value '(1 2 3)
                                    :doc "A set documentation"))
       (node5 (set-declaration-node 'S :value (list (range 1 3))))
       (node6 (set-declaration-node 'S :data-reader 8))
       (node7 (set-declaration-node 'S :data-reader 8 :current-value 19))

       (tab-pos 27))

  (bformat t "Testing set-declaration node")
  (format t "param decl:~vt ~A~%" tab-pos node1)
  (format t "param decl with domain:~vt ~A~%" tab-pos node2)
  (format t "with two indexes:~vt ~A~%" tab-pos node3)
  (format t "with value and doc:~vt ~A~%" tab-pos  node4)
  (format t "with range in values:~vt ~A~%" tab-pos node5)
  (format t "with data-reader:~vt ~A~%" tab-pos node6)
  (format t "with data-reader:~vt ~A~%" tab-pos node7))

(let* ((node1 (set S1))
       (node2 (set S2 :dimension 2))
       (node3 (set S3 :value '(1 2 3)))
       (node4 (set S4 :value '(1 2 3)
                   :doc "A set documentation"))
       (node5 (set S5 :value (range 1 3)
                   :data-reader 8)))

  (bformat t "Testing set-declaration macro")
  (format t "set decl:~40t ~A~%" node1)
  (format t "set decl with domain:~40t ~A~%" node2)
  (format t "with two indexes:~40t ~A~%" node3)
  (format t "with value:~40t ~A~%" node4)
  (format t "with data reader:~40t ~A~%" node5)

  (bformat t "Now testing the set-refs")
  (format t "S1: ~a~%" S1)
  (format t "S2: ~a~%" S2)
  (format t "S3: ~a~%" S3)
  (format t "S4: ~a~%" S4)
  (format t "S5: ~a~%" S5)

  (bformat t "Testing the original-ref")
  (format t "S1: ~a~%" (original-declaration S1))
  (format t "S2: ~a~%" (original-declaration S2))
  (format t "S3: ~a~%" (original-declaration S3))
  (format t "S4: ~a~%" (original-declaration S4))
  (format t "S5: ~a~%" (original-declaration S5))
  )

(let* ((v (var-ref "x"))
       (node1 (display 1))
       (node2 (display v))
       (node3 (display (+ 1 v))))
  (bformat t "Testing display-node")
  (format t "~a~%" node1)
  (format t "~a~%" node2)
  (format t "~a~%" node3))

(let* ((node1 (solve)))
  (bformat t "Testing solve-node")
  (format t "~a~%" node1))

(let* ((param-decl-1 (parameter-declaration-node "P"))
       (p (param-ref "P" param-decl-1))
       (node1 (set-value-of-param p 5))
       (node2 (set-value-of-param p 5 8))
       (node3 nil)
       )
  (bformat t "Testing set-value-of-param")
  (format t "~a~%" p)
  (format t "~a~%" node1)
  (format t "~%Setting a value:~%")
  (format t "value of original declaration (should be nil): ~a~%"
          (value (original-declaration p)))
  (format t "current-value of original declaration (non nil): ~a~%"
          (current-value (original-declaration p)))
  (format t "~a~%" node2)

  (format t "~%setting a list:~%")
  (setf node3 (set-value-of-param p '(1 2 3 4)))
  (format t "~a~%" node3)
  (format t "value of original declaration (should be nil): ~a~%"
          (value (original-declaration p)))
  (format t "current-value of original declaration (non nil): ~a~%"
          (current-value (original-declaration p)))
  )

(let* ((set-decl-1 (set-declaration-node "S"))
       (p (set-ref "S" set-decl-1))
       (node1 (set-value-of-set p 5))
       (node2 (set-value-of-set p 5 8))
       (node3 nil)
       (node4 nil)
       )
  (bformat t "Testing set-value-of-set")
  (format t "~a~%" p)
  (format t "~a~%" node1)

  (format t "~%Setting a value:~%")
  (format t "value of original declaration (expect nil): ~a~%"
          (value (original-declaration p)))
  (format t "current-value of original declaration (non nil): ~a~%"
          (current-value (original-declaration p)))
  (format t "The assignment: ~a~%" node2)

  (format t "~%Setting a list:~%")
  (setf node3 (set-value-of-set p '(1 2 3 4)))
  (format t "~a~%" node3)
  (format t "value of original declaration (expect nil): ~a~%"
          (value (original-declaration p)))
  (format t "current-value of original declaration (non nil): ~a~%"
          (current-value (original-declaration p)))

  (format t "~%Setting a range:~%")
  (setf node4 (set-value-of-set p (range 1 5)))
  (format t "~a~%" node4)
  (format t "value of original declaration (expect nil): ~a~%"
          (value (original-declaration p)))
  (format t "current-value of original declaration (non nil): ~a~%"
          (current-value (original-declaration p))))

(let* ((p1-decl (parameter-declaration-node "p"))
       (p1 (param-ref "p" p1-decl))
       (s-decl (set-declaration-node "S"))
       (s (set-ref "S" s-decl))
       (node1 (data-section-node))
       (set-p (set-value-of-param p1 5))
       (node2 (data-section-node set-p))
       (set-s (set-value-of-set s '(1 2 3)))
       (node3 (data-section-node set-s))
       (node4 (data-section-node set-p set-s))
       )
  (bformat t "Testing data")
  (format t "~a~%" node1)
  (format t "~a~%" node2)
  (format t "~a~%" node3)
  (format t "~a~%" node4)
  (bformat t "Testing is-a-data-section")
  (format t "Expect non nil: ~a~%" (is-a-data-section node1))
  (format t "Expect non nil: ~a~%" (is-a-data-section node4))
  (format t "Expect     nil: ~a~%" (is-a-data-section 5))
  (format t "Expect     nil: ~a~%" (is-a-data-section s)))

(let* ((p1-decl (parameter-declaration-node "p"))
       (p1 (param-ref "p" p1-decl))
       (s-decl (set-declaration-node "S"))
       (s (set-ref "S" s-decl))
       (node1 (data-section))
       (set-p (set-value-of-param p1 5))
       (node2 (data-section set-p))
       (set-s (set-value-of-set s '(1 2 3)))
       (node3 (data-section set-s))
       (node4 (data-section set-p set-s))
       )
  (bformat t "Testing data")
  (format t "~a~%" node1)
  (format t "~a~%" node2)
  (format t "~a~%" node3)
  (format t "~a~%" node4))

(progn
  (bformat t "Testing binary-operators layer 2:")
  (format t "Add:~6t ~a~%"  (+ 1 2))
  (format t "subs:~6t ~a~%" (- 1 2))
  (format t "mult:~6t ~a~%" (* 1 2))
  (format t "div:~6t ~a~%"  (/ 1 2))
  (format t "<:~6t ~a~%"    (< 1 2))
  (format t "<=:~6t ~a~%"   (<= 1 2))
  (format t ">:~6t ~a~%"    (> 1 2))
  (format t ">=:~6t ~a~%"   (>= 1 2))
  (format t "=:~6t ~a~%"    (= 1 2))
  )

(progn
  (bformat t "Testing binary-operators layer 2:")
  (format t "subset:~6t ~a~%"  (subset 1 2))
  (format t "not-equal:~6t ~a~%" (!= 1 2))
  (format t "belongs-to:~6t ~a~%" (in 1 2))
  )

(let* ((node1 (constraint 1 (less-than-op 'x 5)))
       (node2 (constraint 2 (less-than-op 'x 5)
                               :quantifiers
                               ((for-all-quantifier x 'F))))
       (node3 (constraint 3 (less-than-op 'x 'z)
                               :quantifiers
                               ((for-all-quantifier x 'Y nil)
                                (for-all-quantifier z 'Y nil))))
       ;; (node4 (constraint-node 4 (less-than-op 'x 'z)
       ;;                         :quantifiers
       ;;                         (for-all-quantifier 'x 'Y nil)
       ;;                         :doc "A constraint"))
       ;; (node5 (constraint-node 4 (less-than-op 'x 'z)
       ;;                         :quantifiers
       ;;                         (list
       ;;                          (for-all-quantifier 'x 'Y nil)
       ;;                          (< 'x 3))))
       )
  (bformat t "Testing constraint node (layer 2)")
  (format t "simple constraint: ~A~%" node1)
  (format t "constraint with:   ~A~%" node2)
  (format t "more quantifiers:  ~A~%" node3)
  ;; (format t "documentation:      ~A~%" node4)
  ;; (format t "additional cond:    ~A~%" node5)
  )

(let* ((node (for-all-quantifier 'i 'J (greater-than-op 'i 3))))
  (format t "forall with predicate:~% ~a~%" node))

(let* ()
  (bformat t "Testing value for number")
  (format t "Value of number: ~a~%" (value 18)))

(let* ((param-decl (parameter-declaration-node
                    "p1" :value 16))
       (p1 (param-ref "p1" param-decl)))

  (bformat t "Testing value for param-ref")
  (format t "Value of param-ref: ~a~%" (value p1)))

(let* ((param-decl (parameter-declaration-node
                    "p1" :value 16))
       (p1 (param-ref "p1" param-decl)))

  (bformat t "Testing value for param-ref")
  (format t "Value of param-ref: ~a~%" (value p1)))

(let* ((param-decl (parameter-declaration-node
                    "p1" :value 16))
       (p1 (param-ref "p1" param-decl))
       (node1 (add-op 1 3))
       (node2 (subs-op 1 3))
       (node3 (mult-op 1 3))
       (node4 (div-op 1 3))
       (node5 (add-op p1 3))
       (node6 (mult-op 3 p1))
       (node7 (subs-op p1 p1))

       )

  (bformat t "Testing value for binary-ops with 1 op 3")
  (format t "Value of +: ~2d~%" (value node1))
  (format t "Value of -: ~2d~%" (value node2))
  (format t "Value of *: ~2d~%" (value node3))
  (format t "Value of /: ~2d~%" (value node4))

  (bformat t "Testing value for binary-ops with p1 with value ~a"
           (value p1))
  (format t "Value of p1 +  3: ~2d~%" (value node5))
  (format t "Value of  3 * p1: ~2d~%" (value node6))
  (format t "Value of p1 - p1: ~2d~%" (value node7))
  )

(let* ()
  (bformat t "Testing value for number")
  (format t "current-value of number: ~a~%" (current-value 18)))

(let* ((param-decl (parameter-declaration-node
                    "p1" :value 16))
       (p1 (param-ref "p1" param-decl))
       (param-decl2 (parameter-declaration-node
                     "p2" :value 16
                     :current-value 14))
       (p2 (param-ref "p2" param-decl2)))

  (bformat t "Testing current-value for param-ref")
  (format t "current-value of param-ref: ~a~%" (current-value p1))
  (format t "current-value of param-ref: ~a~%" (current-value p2)))

(let* ((param-decl (parameter-declaration-node
                    "p1" :value 16))
       (p1 (param-ref "p1" param-decl))
       (param-decl2 (parameter-declaration-node
                          "p2" :value 16
                          :current-value 14))
       (p2 (param-ref "p2" param-decl2)))

  (bformat t "Testing current-value for param-ref")
  (format t "Value of param-ref: ~a~%" (current-value p1))
  (format t "Value of param-ref: ~a~%" (current-value p2)))

(let* ((param-decl (parameter-declaration-node
                    "p1" :value 16
                    :current-value 16))
       (p1 (param-ref "p1" param-decl))
       (node1 (add-op 1 3))
       (node2 (subs-op 1 3))
       (node3 (mult-op 1 3))
       (node4 (div-op 1 3))
       (node5 (add-op p1 3))
       (node6 (mult-op 3 p1))
       (node7 (subs-op p1 p1))

       )

  (bformat t "Testing current-value for binary-ops with 1 op 3")
  (format t "Value of +: ~2d~%" (current-value node1))
  (format t "Value of -: ~2d~%" (current-value node2))
  (format t "Value of *: ~2d~%" (current-value node3))
  (format t "Value of /: ~2d~%" (current-value node4))

  (bformat t "Testing value for binary-ops with p1 with value ~a"
           (current-value p1))
  (format t "Value of p1 +  3: ~2d~%" (current-value node5))
  (format t "Value of  3 * p1: ~2d~%" (current-value node6))
  (format t "Value of p1 - p1: ~2d~%" (current-value node7))
  )

(let* ((param-decl (parameter-declaration-node
                    "p1" :value 16))
       (p1 (param-ref "p1" param-decl))
       (s-decl (set-declaration-node "S" :value '(a b c)))
       (s (set-ref "S" s-decl))
       (param-decl2 (parameter-declaration-node
                     "p2" :value {1 2 3} :domain s))
       (p2 (param-ref "p2" param-decl2)))

  (bformat t "Testing value for param-ref")
  (format t "domain of param-ref: ~a~%" (domain p1))
  (format t "domain of param-ref: ~a~%" (domain p2))
  (format t "value of domain of param-ref: ~a~%" (value (domain p2))))

(let* ((v1-decl (variable-declaration-node
                 "v1"))
       (v1 (var-ref "v1" v1-decl))
       (s-decl (set-declaration-node "S" :value '(a b c)))
       (s (set-ref "S" s-decl))
       (v2-decl (variable-declaration-node
                 "v2" :domain s))
       (v2 (var-ref "v2" v2-decl))
       )

  (bformat t "Testing domain for param-ref")
  (format t "domain of param-ref: ~a~%" (domain v1))
  (format t "domain of param-ref: ~a~%" (domain v2))
  (format t "value of domain of param-ref: ~a~%" (value (domain v2)))
  )

(let* ()
  (bformat t "Testing name for number")
  (format t "Value of number: ~a~%" (name 18))
  (format t "Value of number: ~a~%" (name 111330))
  (format t "Value of number: ~a~%" (name -111330)))

(let* ()
  (bformat t "Testing name for number")
  (format t "Value of number: ~a~%" (name 'hello))
  (format t "Value of number: ~a~%" (name 'hello-world))
  (format t "Value of number: ~a~%" (name 'hello-111330)))

(let* ()
  (bformat t "Testing name for number")
  (format t "Value of number: ~a~%" "hello")
  (format t "Value of number: ~a~%" "hello-world")
  (format t "Value of number: ~a~%" "hello-111330"))

(let* ((set-decl-1 (set-declaration-node "S"))
        (p (set-ref "S" set-decl-1))
        (node1 (set-value-of-set p 5))
        (node2 (set-value-of-set p 5 8))
        (node3 nil)
        (node4 nil)
        )
   (bformat t "Testing name for set-value-of-set")
   (format t "Expect S: ~a~%" (name p))
   (format t "~a~%" node1)
   (format t "Expect S: ~a~%" (name node1))
   (format t "Expect S: ~a~%" (name node2))

   (setf node3 (set-value-of-set p '(1 2 3 4)))
   (format t "~a~%" node3)
   (format t "Expect S: ~a~%" (name node3))
   (setf node4 (set-value-of-set p (range 1 5)))
   (format t "~a~%" node4)
   (format t "Expect S: ~a~%" (name node4)))

(let* ((param-decl-1 (parameter-declaration-node "P"))
        (p (param-ref "P" param-decl-1))
        (node1 (set-value-of-param p 5))
        (node2 (set-value-of-param p 5 8)))
   (bformat t "Testing name of set-value-of-param")
   (format t "Expect P: ~a~%" (name p))

   (format t "Expect P: ~a~%" (name node1))

   (setf node3 (set-value-of-param p '(1 2 3 4)))
   (format t "Expect P: ~a~%" (name node2)))

(let* ((node1 '(1 2 3 4 5))
       (node2 (list 1 2 3 4 5 6 7 8))
       (dreader +standard-data-reader+))

  (bformat t "Testing generate-value-of-set with list")
  (format t "~a~%" (generate-value-of-set node1 dreader)) (terpri)
  (format t "~a~%" (generate-value-of-set node2 dreader)) (terpri)
  )

(let* ((param-decl (parameter-declaration-node "p1" :value 5))
       (p1 (param-ref "p1" param-decl))
       (node1 (range 1 5))
       (node2 (range 1 10 :increment 2))
       (node3 (range 5  1 :increment -1))
       (node4 (range 1 (+ 5 5)))
       (node5 (range (- 3 1) (+ 5 5)))
       (node6 (range p1 (+ 5 5)))
       (node7 (range 1 p1))
       (node8 (range 1 (* p1 2)))
       (dreader +standard-data-reader+))

  (bformat t "Testing generate-value-of-set with range-node")
  (format t "~a~%" (generate-value-of-set node1 dreader))
  (format t "~a~%" (generate-value-of-set node2 dreader))
  (format t "~a~%" (generate-value-of-set node3 dreader))
  (format t "~a~%" (generate-value-of-set node4 dreader))
  (format t "~a~%" (generate-value-of-set node5 dreader))
  (format t "~a~%" (generate-value-of-set node6 dreader))
  (format t "~a~%" (generate-value-of-set node7 dreader))
  (format t "~a~%" (generate-value-of-set node8 dreader))
  )

(let* ((name-generator (make-dummy-variable-name-generator t)))

  (bformat t "Testing make-dummy-variable-name-generator")
  (dotimes (i 12)
   (format t "~a~%" (funcall name-generator))))

(format t "~s~%" (make-slots-for-basic-language
                  `((add "+" "addition")
                    (subs "-" "substraction")
                    (weird nil "weird op"))))

(define-basic-language test-language ()
  ((add "+" "addition")
   (subs "-" "substraction")
   (weird nil "weird op")))

(let* ((lang (make-instance 'basic-language)))
  (bformat t "Testing basic-language operators:")
  (format t "+:   ~a~%" (add-symbol lang))
  (format t "-:   ~a~%" (subs-symbol lang))
  (format t "*:   ~a~%" (mult-symbol lang))
  (format t "/:   ~a~%" (div-symbol lang))
  (format t "<:   ~a~%" (less-than-symbol lang))
  (format t "<=:  ~a~%" (less-or-equal-symbol lang))
  (format t ">:   ~a~%" (greater-than-symbol lang))
  (format t ">=   ~a~%" (greater-or-equal-symbol lang))
  (format t "=    ~a~%" (equality-symbol lang))
  (format t "nil: ~a~%" (assignment-symbol lang))
  (format t "nil: ~a~%" (union-op-symbol lang))
  (format t "nil: ~a~%" (intersection-symbol lang))
  (format t "nil: ~a~%" (difference-symbol lang))
  (format t "nil: ~a~%" (cartesian-product-symbol lang)))

(let* ((lang (make-instance 'basic-language)))
  (bformat t "Testing code-generation for numbers:")
  (generate-code 4 lang t))

(let* ((lang (make-instance 'basic-language)))
  (bformat t "Testing code-generation for strings:")
  (generate-code "hello" lang t))

(let* ((lang (make-instance 'basic-language)))
  (bformat t "Testing code-generation for lists:")
  (generate-code `(1 2 "three") lang t))

(let* ((lang (make-instance 'basic-language)))
  (bformat t "Testing code-generation for instructions-lists:")
  (generate-code (instructions-list 1 2 "three") lang t))

(let* ((lang (make-instance 'infix-language)))
  (bformat t "Testing code-generation for infix-language")
  (generate-code (+  1 2) lang t) (terpri) 
  (generate-code (<  1 2) lang t) (terpri)
  (generate-code (<= 1 2) lang t) (terpri)
  (generate-code (>  1 2) lang t) (terpri)
  (generate-code (>= 1 2) lang t) (terpri)
  (generate-code (=  1 2) lang t) (terpri)
  (bformat t "Testing the addition of the extra parenthesis")
  (generate-code (+  (+ 1 1) (- 3 2)) lang t) (terpri) 
  (generate-code (<  (- 1 3) (+ 2 4)) lang t) (terpri)
  (generate-code (<= (- 1 3) (+ 2 4)) lang t) (terpri)
  (generate-code (>  (- 1 3) (+ 2 4)) lang t) (terpri)
  (generate-code (>= (- 1 3) (+ 2 4)) lang t) (terpri)
  (generate-code (=  (- 1 3) (+ 2 4)) lang t) (terpri)
  )

(let* ((lang (make-instance 'infix-language)))
  (bformat t "Testing code-generation for infix-language")
  (generate-code (*  1 2) lang t) (terpri) 
  (generate-code (*  (+ 1 3) 2) lang t) (terpri)
  (generate-code (*  1 (+ 2 3)) lang t) (terpri)
  (generate-code (* (+ 1 3) (+ 2 4)) lang t) (terpri))

(let* ((lang (make-instance 'infix-language)))
  (bformat t "Testing code-generation for div node in infix-language")
  (generate-code (/  1 2) lang t) (terpri) 
  (generate-code (/  (+ 1 3) 2) lang t) (terpri)
  (generate-code (/  1 (+ 2 3)) lang t) (terpri)
  (generate-code (/ (+ 1 3) (+ 2 4)) lang t) (terpri)
  (generate-code (/ (* 1 3) 2) lang t) (terpri)
  (generate-code (/ 1 (* 2 4)) lang t) (terpri)
  (generate-code (/ (* 1 3) (/ 2 4)) lang t) (terpri)
  (generate-code (/ (/ 1 3) 2) lang t) (terpri))

(let* ((lang (make-instance 'infix-language)))
  (bformat t "Testing code-generation for div node in infix-language")
  (generate-code (-  1 2) lang t) (terpri) 
  (generate-code (-  (+ 1 3) 2) lang t) (terpri)
  (generate-code (-  1 (+ 2 3)) lang t) (terpri)
  (generate-code (- (+ 1 3) (+ 2 4)) lang t) (terpri))

(progn
  (defclass disposable-test-class2 (infix-language
                                    symbol-separated-language)
    ())
  (node-ends-with-separator add-op disposable-test-class)
  (let* ((lang (make-instance 'disposable-test-class)))
    (bformat t "Testing code-generation for symbol-separated-language")
    (generate-code (+ 1 2) lang t)))

(progn
  (bformat t "Testing symbol-to-camelcase")
  (format t "~a~%" (symbol-to-camelcase 'hello-world))
  (format t "~a~%" (symbol-to-camelcase 'hello))
  (format t "~a~%" (symbol-to-camelcase 'h)))

(let* ((lang (make-instance 'camel-case-language)))
  (progn
    (bformat t "Testing symbol-to-camelcase")
    (generate-code 'hello-world lang t) (terpri)
    (generate-code 'hello lang t) (terpri)
    (generate-code 'h lang t) (terpri)))

(progn
  (bformat t "Testing symbol-to-underscore")
  (format t "~a~%" (convert-to-underscore 'hello-world))
  (format t "~a~%" (convert-to-underscore 'hello))
  (format t "~a~%" (convert-to-underscore 'h)))

(progn
  (bformat t "Testing string-to-underscore")
  (format t "~a~%" (convert-to-underscore "hello-WORLD"))
  (format t "~a~%" (convert-to-underscore "hello"))
  (format t "~a~%" (convert-to-underscore "HELLO_WORLD")))

(let* ((lang (make-instance 'underscore-language)))
  (progn
    (bformat t "Testing symbol-to-underscore")
    (generate-code 'hello-world lang t) (terpri)
    (generate-code 'hello lang t) (terpri)
    (generate-code 'h lang t) (terpri)))

(let* ((lang (make-instance 'underscore-language)))
  (progn
    (bformat t "Testing write-name for underscore language")
    (write-name (var-ref 'hello-WORLD) lang t) (terpri)
    (write-name (variable-declaration-node 'Z-m-R) lang t) (terpri)
    (write-name (set-ref 's-1) lang t) (terpri)
    (write-name (set-declaration-node 'Set-1) lang t) (terpri)
    (write-name (param-ref 'p-1) lang t) (terpri)
    (write-name (parameter-declaration-node 'param-1) lang t) (terpri)
    ))

(LET* ((lang (MAKE-INSTANCE 'UNDERSCORE-LANGUAGE)))
  (PROGN
    (BFORMAT T "Testing write-name for underscore language")
    (WRITE-NAME (VAR-REF 'hello-WORLD) lang T) (TERPRI)
    (WRITE-NAME (VARIABLE-DECLARATION-NODE 'Z-m-R) lang T) (TERPRI)
    (WRITE-NAME (SET-REF 's-1) lang T) (TERPRI)
    (WRITE-NAME (SET-DECLARATION-NODE 'Set-1) lang T) (TERPRI)
    (WRITE-NAME (PARAM-REF 'p-1) lang T) (TERPRI)
    (WRITE-NAME (PARAMETER-DECLARATION-NODE 'param-1) lang T) (TERPRI)
    ))

(let* ((lang (make-instance 'variable-lowercase-language)))
  (progn
    (bformat t "Testing variable-lowercase")
    (write-name (var-ref 'hello-WORLD) lang t) (terpri)
    (write-name (var-ref 'hello) lang t) (terpri)
    (write-name (var-ref 'h) lang t) (terpri)))

(let* ((lang (make-instance 'variable-lowercase-language)))
  (progn
    (bformat t "Testing variable-lowercase-language")
    (let* ((node1 (variable-declaration-node 'x))
           (node2 (variable-declaration-node 'Y :var-type binary-variable))
           (node3 (variable-declaration-node 'T
                                             :var-type integer-variable
                                             :lower-bound 1))
           (node4 (variable-declaration-node 'Z123
                                             :var-type binary-variable
                                             :lower-bound 1
                                             :upper-bound 10))
           (node5 (variable-declaration-node 'T-x :var-type integer-variable
                                             :lower-bound 1
                                             :upper-bound 10
                                             :domain 'H))
           (node6 (variable-declaration-node 'X_ij :var-type continuous-variable
                                             :lower-bound 1
                                             :upper-bound 10
                                             :domain `(H H)))
           (node7 (variable-declaration-node 'Z-X-MM
                                             :var-type binary-variable
                                             :doc "A binary variable"))
           )

    (write-name node1 lang t) (terpri)
    (write-name node2 lang t) (terpri)
    (write-name node3 lang t) (terpri)
    (write-name node4 lang t) (terpri)
    (write-name node5 lang t) (terpri)
    (write-name node6 lang t) (terpri)
    (write-name node7 lang t) (terpri)
    )))

(let* ((lang (make-instance 'set-uppercase-language)))
  (progn
    (bformat t "Testing set-uppercase-language")
    (generate-code (set-ref 'hello-WORLD) lang t) (terpri)
    (generate-code (set-ref 'hello) lang t) (terpri)
    (generate-code (set-ref 'h) lang t) (terpri)))

(let* ((lang (make-instance 'set-uppercase-language)))
  (progn
    (bformat t "Testing set-uppercase")
    (write-name (set-ref 'hello-world) lang t) (terpri)
    (write-name (set-ref 'hello) lang t) (terpri)
    (write-name (set-ref 'h) lang t) (terpri)))

(let* ((lang (make-instance 'set-uppercase-language)))
  (progn
    (bformat t "Testing set-uppercase-language")
    (let* ((node1 (set-declaration-node 's))
           (node2 (set-declaration-node 's1 :dimension 2))
           (node3 (set-declaration-node 'S-12 :value '(1 2 3)))
           (node4 (set-declaration-node 'S_ij :value '(1 2 3)
                                        :doc "A set documentation"))
           (node5 (set-declaration-node 'xww :value (list (range 1 3)))))

    (write-name node1 lang t) (terpri)
    (write-name node2 lang t) (terpri)
    (write-name node3 lang t) (terpri)
    (write-name node4 lang t) (terpri)
    (write-name node5 lang t) (terpri)
    )))

(let* ((lang (make-instance 'variable-lowercase-language)))
  (progn
    (bformat t "Testing variable-lowercase")
    (generate-code (var-ref 'hello-WORLD) lang t) (terpri)
    (generate-code (var-ref 'hello) lang t) (terpri)
    (generate-code (var-ref 'h) lang t) (terpri)))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing operations in gmpl-language.")
    (generate-code (union-op 'A 'B) lang t) (terpri)
    (generate-code (intersection-op 'A 'B) lang t) (terpri)
    (generate-code (assignment-op 'A 'B) lang t) (terpri)
    (generate-code (difference-op 'A 'B) lang t) (terpri)
    (generate-code (cartesian-product-op 'A 'B) lang t) (terpri)))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing is-a-reference in gmpl-language.")
    (generate-code (var-ref "A") lang t) (terpri)))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing index-at in gmpl-language.")
    (generate-code (index-at "x" 1 2 3) lang t) (terpri)))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing for-all in gmpl-language.")
    (generate-code (for-all-quantifier  "i" "I" nil) lang t) (terpri)
    (generate-code (for-all-quantifier  "i" "I" (< "i" 5)) lang t) (terpri)))

(let* ((lang (gmpl-language))
       (node1 (var-in-set 'i 'J))
       (node2 (var-in-set "i" "J"))
       (i (var-ref "i"))
       (s-decl (set-declaration-node "S"))
       (S (set-ref "S" s-decl))
       (node3 (var-in-set i S))
       )
  (progn
    (bformat t "Testing sum-node in gmpl-language.")
    (generate-code node1 lang t)  (terpri)
    (generate-code node2 lang t)  (terpri)
    (generate-code node3 lang t)  (terpri)
    ))

(let* ((lang (gmpl-language))
       (node1 (var-from-to 'i 1 10))
       )
  (progn
    (bformat t "Testing sum-node in gmpl-language.")
    (generate-code node1 lang t)  (terpri)
    ))

(let* ((lang (gmpl-language))
       (s-one (set-ref 's-one))
       (node1 (sum-node (list (var-in-set i s-one)) "i"))
       (node2 (sum-node (list (var-in-set i s-one)
                              (var-in-set j s-one))
                        (+ 'i 'j)))
       )
  (progn
    (bformat t "Testing sum-node in gmpl-language.")
    (generate-code node1 lang t) (terpri)
    (generate-code node2 lang t) (terpri)
    ))

(let* ((lang (gmpl-language))
       (s-one (set-ref 's-one))
       (node1 (sumf ((i in s-one)) i))
       (node2 (sumf ((i in s-one)
                     (j in s-one))
                    (+ i j)))
       (node3 (sumf ((i in s-one)
                     (< i 5))
                    (+ i 2)))
       )
  (progn
    (bformat t "Testing sum-node in gmpl-language.")
    (generate-code node1 lang t) (terpri)
    (generate-code node2 lang t) (terpri)
    (generate-code node3 lang t) (terpri)
    ))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing sum-node in gmpl-language.")
    (generate-code (sumf ((i from 1 to 5)) 'i) lang t)
    (terpri)))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing constraint-node in gmpl-language.")
    (generate-code (constraint-node 1 (<= "x" 5)) lang t)
    (terpri)
    (generate-code (constraint-node "r" (<= (index-at "x" 1 "j") 5)
                                    :quantifiers
                                    (list (for-all-quantifier "j" "J" nil)))
                   lang t)
    (terpri)
    (generate-code (constraint-node "r" (<= (index-at "x" 1 "j" "k") 5)
                                    :quantifiers
                                    (list (for-all-quantifier "j" "J" nil)
                                          (for-all-quantifier "k" "K" nil)))
                   lang t)
    (terpri)
    ))

(let* ((lang (make-instance 'gmpl-language)))
  (bformat t "Finding bug in code-generation for constraint")
  (set i1 :value (range 1 5))
  (variable-declaration y1 :domain {i1 i1})
  (setf c1 (constraint "c1" (= (sumf ((b in I1)) [y1 0 b]) 5)))

  (generate-code c1 lang t)
  )

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing continuous-variable-type in gmpl-language.")
    (generate-code (continuous-variable-type) lang t)
    (terpri)))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing integer-variable-type in gmpl-language.")
    (generate-code (integer-variable-type) lang t)
    (terpri)))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing binary-variable-type in gmpl-language.")
    (generate-code (binary-variable-type) lang t)
    (terpri)))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing variable-declaration-node in gmpl-language.")
    (let* ((node1 (variable-declaration-node 'x))
           (node2 (variable-declaration-node 'Y :domain '(I)))
           (node3 (variable-declaration-node 'Z12 :domain '(I I)))
           (node4 (variable-declaration-node 'TEMP :domain '(I I)
                                             :var-type integer-variable))
           (node5 (variable-declaration-node 'x123 :domain '(I I)
                                             :var-type binary-variable))
           (node6 (variable-declaration-node 'x-f-y :domain '(I I)
                                             :var-type continuous-variable))
           (node7 (variable-declaration-node 'x-1234 :domain '(I I)
                                             :var-type continuous-variable
                                             :lower-bound 1))
           (node8 (variable-declaration-node 'x_ij :domain '(I I)
                                             :var-type continuous-variable
                                             :upper-bound 10))
           (node9 (variable-declaration-node 'Q_mn :domain '(I I)
                                             :var-type continuous-variable
                                             :lower-bound 5
                                             :upper-bound 10)))
      (generate-code node1 lang t) (terpri)
      (generate-code node2 lang t) (terpri)
      (generate-code node3 lang t) (terpri)
      (generate-code node4 lang t) (terpri)
      (generate-code node5 lang t) (terpri)
      (generate-code node6 lang t) (terpri)
      (generate-code node7 lang t) (terpri)
      (generate-code node8 lang t) (terpri)
      (generate-code node9 lang t) (terpri))))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing objective-function in gmpl-language.")
    (let* ((node1 (objective-function-node "goal" (+ "x" 5) :id "obj1"))
           (node2 (minimize "obj2" (+ (* "x" 5) (* "y" 6))))
           (node3 (maximize "obj3" (+ (* "x" 5) (* "y" 6)))))
      (generate-code node1 lang t) (terpri)
      (generate-code node2 lang t) (terpri)
      (generate-code node3 lang t) (terpri))))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing problem-definition-node in gmpl-language.")
    (let* ((p1 (problem-node "example 1"  1 2 3 4))
           (p2 (problem-node "example 2"  1 2 (data-section 'a 'b) 3 4)))
      (generate-code p1 lang t) (terpri)
      (generate-code p2 lang t) (terpri))))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing an actual problem in gmpl-language.")
    (let* ()
      (generate-code example1 lang t))))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing problem-definition-node in gmpl-language.")
    (let* ((p1 (problem-node "example 1"  1 2 3 4)))
      (generate-code p1 lang t))))

(let* ((lang (gmpl-language)))
  (progn
    (bformat t "Testing an actual problem in gmpl-language.")
    (let* ()
      (generate-code example1 lang t))))

(let* ((lang (gmpl-language))
       (node1 (parameter-declaration-node 'p))
       (node2 (parameter-declaration-node 'P :domain '(I)))
       (node3 (parameter-declaration-node 'P :domain '(I J)))
       (node4 (parameter-declaration-node 'P :value 5))
       (node5 (parameter-declaration-node 'P :value 5
                                          :doc "A documentation")))
  (bformat t "Testing parameter-declaration node")
  (generate-code node1 lang t) (terpri) 
  (generate-code node2 lang t) (terpri)
  (generate-code node3 lang t) (terpri)
  (generate-code node4 lang t) (terpri)
  (generate-code node5 lang t) (terpri))

(let* ((lang (gmpl-language))
       (node1 (parameter p))
       (node2 (parameter P :domain '(I)))
       (node3 (parameter P :domain '(I J)))
       (node4 (parameter P :value 5))
       (node5 (parameter P :value 5
                         :doc "A documentation")))
  (bformat t "Testing parameter-declaration node")
  (generate-code node1 lang t) (terpri) 
  (generate-code node2 lang t) (terpri)
  (generate-code node3 lang t) (terpri)
  (generate-code node4 lang t) (terpri)
  (generate-code node5 lang t) (terpri))

(let* ((lang (gmpl-language))
       (s1-decl (set-declaration-node "s-test"))
       (s1 (set-ref "s-test" s1-decl))
       (node1 (parameter p))
       (node2 (parameter P :domain (list s1)))
       (node3 (parameter P :domain (list s1 s1)))
       (node4 (parameter P :value 5))
       (node5 (parameter P :value 1
                         :doc "A documentation")))
  (bformat t "Testing parameter-declaration node")
  (generate-code node1 lang t) (terpri) 
  (generate-code node2 lang t) (terpri)
  (generate-code node3 lang t) (terpri)
  (generate-code node4 lang t) (terpri)
  (generate-code node5 lang t) (terpri))

(let* ((lang (gmpl-language))
       (node1 (range 1 5))
       (node2 (range 1 5 :increment 2)))

  (bformat t "Testing code-generation for range-node")
  (generate-code node1 lang t) (terpri)
  (generate-code node2 lang t) (terpri))

(let* ((lang (gmpl-language))
       (node1 (set-declaration-node 's))
       (node2 (set-declaration-node 'S1 :dimension 2))
       (node3 (set-declaration-node 'S-max :value '(1 2 3)))
       (node4 (set-declaration-node 'xvM :value '(1 2 3)
                                    :doc "A set documentation"))
       (node5 (set-declaration-node 'T12 :dimension (+ 1 1)))
       (node6 (set-declaration-node 'TS_45p :value (list 1 (+ 1 1) 3)))
       (node7 (set-declaration-node 'S12_O-12 :value (list (range 1 3)))))

  (bformat t "Testing set-declaration code generation")
  (generate-code node1 lang t) (terpri)
  (generate-code node2 lang t) (terpri)
  (generate-code node3 lang t) (terpri)
  (generate-code node4 lang t) (terpri)
  (generate-code node5 lang t) (terpri)
  (generate-code node6 lang t) (terpri)
  (generate-code node7 lang t) (terpri))

(let* ((lang (gmpl-language))
       (x (var-ref "x"))
       (node1 (display 1))
       (node2 (display x))
       (node3 (display (+ 1 x))))

  (bformat t "Testing code-generation for display-node")
  (generate-code node1 lang t) (terpri)
  (generate-code node2 lang t) (terpri)
  (generate-code node3 lang t) (terpri))

(let* ((lang (gmpl-language))
       (node1 (solve)))

  (bformat t "Testing code-generation for solve-node")
  (generate-code node1 lang t) (terpri))

(let* ((lang (gmpl-language))
       (node1 (variable-declaration-node "X1"
                                         :doc "A documented variable"))
       (node2 (parameter-declaration-node "p1"
                                         :doc "A documented parameter"))
       (node3 (set-declaration-node "s1"
                                    :doc "A documented set"))
       (node4 (constraint-node "r1" (<= (+ "x" 1) 5)
                                         :doc "A documented constraint")))

  (bformat t "Testing code-generation for has doc")
  (generate-code node1 lang t) (terpri)
  (generate-code node2 lang t) (terpri)
  (generate-code node3 lang t) (terpri)
  (generate-code node4 lang t) (terpri)
  )

(let* ((lang (gmpl-language))
       (dreader +standard-data-reader+)
       (set-decl (set-declaration-node "S1"
                                       :doc "A documented set"))
       (s1 (set-ref "S1" set-decl))
       (node1 (set-value-of-set s1 '(1 2 3 4 5)))
       (node2 (set-value-of-set s1 '(one two three)))
       (node3 (set-value-of-set s1 '("one" "two" "three"))))

  (bformat t "Testing format-value-of set with standard-data-reader")
  (format-value-of node1 (value node1) lang dreader t) (terpri)
  (format-value-of node2 (value node2) lang dreader t) (terpri)
  (format-value-of node3 (value node3) lang dreader t) (terpri)
  )

(let* ((lang (gmpl-language))
       (dreader +standard-data-reader+)
       (set-decl (set-declaration-node "S1"
                                       :doc "A documented set"))
       (s1 (set-ref "S1" set-decl))
       (node1 (set-value-of-set s1 (range 1 5)))
       (node2 (set-value-of-set s1 (range 1 10 :increment 2))))

  (bformat t "Testing format-value-of set with standard-data-reader")
  (format-value-of node1 (value node1) lang dreader t) (terpri)
  (format-value-of node2 (value node2) lang dreader t) (terpri)
  )

(let* ((lang (gmpl-language))
       (dreader +standard-data-reader+)
       (param-decl (parameter-declaration-node "p1"
                                       :doc "A documented param"))
       (p1 (param-ref "p1" param-decl))
       (node1 (set-value-of-param p1 6)))

  (bformat t "Testing format-value-of param with standard-data-reader")
  (format-value-of node1 (value node1) lang dreader t) (terpri)
  )

(let* ((lang (gmpl-language))
       (s (set-declaration-node "S" :value {1 2 3 }
                                :current-value {1 2 3 }))
       (dreader +standard-data-reader+)
       (param-decl (parameter-declaration-node "parametro_importante"
                                               :domain {s}))
       (p1 (param-ref "parametro_importante" param-decl))
       (node1 (set-value-of-param p1 {10 20 30})))

  (bformat t "Testing format-value-of param with standard-data-reader")
  (format-value-of node1 (value node1) lang dreader t) (terpri)
  )

(let* ((lang (gmpl-language))
       (s1-decl (set-declaration-node
                 "S1" :value '(city province country)
                 :current-value '(city province country)))
       (s2-decl (set-declaration-node
                 "S2" :value '(havana miami bombay london)
                 :current-value '(havana miami bombay london)))
       (s1 (set-ref "S1" s1-decl))
       (s2 (set-ref "S2" s2-decl))
       (dreader +standard-data-reader+)
       (param-decl (parameter-declaration-node "parametro_importante"
                                               :domain {s1 s2}))
       (p1 (param-ref "parametro_importante" param-decl))
       (node1 (set-value-of-param p1 '((1 2 3 4)
                                       (5 6 7 8)
                                       (1 3 5 7)))))

  (bformat t "Testing format-value-of param with standard-data-reader")
  (format-value-of node1 (value node1) lang dreader t) (terpri)
  )

(let* ((lang (gmpl-language))
       (set-decl (set-declaration-node "S1"
                                       :doc "A documented set"))
       (s1 (set-ref "S1" set-decl))
       (node1 (set-value-of-set s1 '(1 2 3 4 5)))
       (node2 (set-value-of-set s1 '(one two three)))
       (node3 (set-value-of-set s1 '("one" "two" "three")))
       (node4 (set-value-of-set s1 (range 5 10)))
       (node5 (set-value-of-set s1 (range 5 10 :increment 2))))

  (bformat t "Testing generate-code for set-value-of-set")
  (generate-code node1 lang t) (terpri)
  (generate-code node2 lang t) (terpri)
  (generate-code node3 lang t) (terpri)
  (generate-code node4 lang t) (terpri)
  (generate-code node5 lang t) (terpri)
  )

(let* ((lang (gmpl-language))
       (param-decl (parameter-declaration-node "p1"))
       (p1 (param-ref "p1" param-decl))
       (node1 (set-value-of-param p1 6))
       (sdecl (set-declaration-node "S" :value {1 2 3}
                                    :current-value {1 2 3}))
       (s (set-ref "S" sdecl))
       (param-decl2 (parameter-declaration-node "parameter2"
                                                :domain {s}))
       (parameter2 (param-ref "parameter2" param-decl2))
       (node2 (set-value-of-param parameter2 '(one two three)))
       (param-decl3 (parameter-declaration-node "p3"
                                                :domain {s}))
       (p3 (param-ref "p3" param-decl3))
       (node3 (set-value-of-param p3 '("one" "two" "three")))
       (s2decl (set-declaration-node "S2" :value {1}))
       (s2 (set-ref "S2" s2decl))
       (param-decl4 (parameter-declaration-node "p444"
                                                :domain {s2}))
       (p444 (param-ref "p444" param-decl4))
       (node4 (set-value-of-param p444 {100}))
       (s3decl (set-declaration-node
                "S3" :value {"Habana" "Mayabeque" "Pinar" "Guantanamo"}))
       (s3 (set-ref "S3" s3decl))
       (param-decl5 (parameter-declaration-node "p5"
                                                :domain {s3}))
       (p5 (param-ref "p5" param-decl5))
       (node5 (set-value-of-param p5 {1 5 10 20}))
       )

  (bformat t "Testing generate-code for set-value-of-param")

  (format t "value of sdecl: ~a~%" (current-value sdecl))

  (format t "Just set a number:~%")
  (generate-code node1 lang t) (terpri)
  (format t "~%Assign a list:~%")
  (generate-code node2 lang t) (terpri)(terpri)
  ;; (generate-code node3 lang t) (terpri)(terpri)
  ;; (generate-code node4 lang t) (terpri)(terpri)
  ;; (generate-code node5 lang t) (terpri)(terpri)
  )

(let* ((lang (gmpl-language))
       (s1decl (set-declaration-node
                "S1" :value {"Newton" "Salgari"}
                :current-value {"Newton" "Salgari"}))
       (s1 (set-ref "S1" s1decl))

       (s2decl (set-declaration-node
                "S2"
                :value {"Habana" "Mayabeque" "Pinar" "Guantanamo"}
                :current-value {"Habana" "Mayabeque" "Pinar" "Guantanamo"}))

       (s2 (set-ref "S2" s2decl))
       (param-decl1 (parameter-declaration-node 
                     "p1" :domain {s1 s2}))
       (p1 (param-ref "p1" param-decl1))
       (node1 (set-value-of-param p1 '((1 2 3 4)
                                       (5 6 7 8))))

       (s3decl (set-declaration-node
                "S3" :value {1 2 3}
                :current-value {1 2 3}))
       (s3 (set-ref "S3" s3decl))

       (s4decl (set-declaration-node
                "S4"
                :value {1 2 3 4}
                :current-value {1 2 3 4}))

       (s4 (set-ref "S4" s4decl))

       (param-decl2 (parameter-declaration-node 
                     "p2" :domain {s3 s4}))
       (p2 (param-ref "p2" param-decl2))
       (node2 (set-value-of-param p2 '((1 2 3 4)
                                       (5 6 7 8)
                                       (5 6 7 8))))

       (param-decl3 (parameter-declaration-node 
                     "p3" :domain {s3 s4}))
       (p3 (param-ref "p3" param-decl3))
       (node3 (set-value-of-param p3 '((1.1 2 3    4)
                                       (5   6 7.12 8)
                                       (5   6 7    8.66))))

       )

  (bformat t "Testing generate-code for set-value-of-param with list")
  (generate-code node1 lang t) (terpri)(terpri)
  (generate-code node2 lang t) (terpri)(terpri)
  (generate-code node3 lang t) (terpri)(terpri)
  )

(let* ((p1-decl (parameter-declaration-node "p"))
       (p1 (param-ref "p" p1-decl))
       (s-decl (set-declaration-node "S"))
       (s (set-ref "S" s-decl))
       (node1 (data-section))
       (set-p (set-value-of-param p1 5))
       (node2 (data-section set-p))
       (set-s (set-value-of-set s '(1 2 3)))
       (node3 (data-section set-s))
       (node4 (data-section set-p set-s))
       )
  (bformat t "Testing code generation for data section")
  (format t "Testing empty data section:~%")
  (generate-code node1 (gmpl-language) t) (terpri)
  (format t "Testing data section with a param:~%")
  (generate-code node2 (gmpl-language) t) (terpri)
  (format t "Testing data section with a set:~%")
  (generate-code node3 (gmpl-language) t) (terpri)
  (format t "Testing data section with a set and a param:~%")
  (generate-code node4 (gmpl-language) t) (terpri)
  )

(let* ((lang (gmpl-language))
       (s (set-declaration-node "S" :value {1 2 3 }))
       (dreader +standard-data-reader+)
       (param-decl (parameter-declaration-node
                    "parametro_importante"
                    :domain {s}))
       (p1 (param-ref "parametro_importante" param-decl))
       (node1 (set-value-of-param p1 {10 20 30})))

  (bformat t "Testing format-params-values-as-list")
  (format-params-values-as-list p1 {10 20 30} lang t)

  )
