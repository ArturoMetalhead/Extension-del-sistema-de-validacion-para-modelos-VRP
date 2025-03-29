(in-package :lmml)

(defclass gmpl-language (infix-language
                         symbol-separated-language
                         underscore-language
                         ;; variable-lowercase-language
                         ;; set-uppercase-language
                         )
  ())

(defun gmpl-language ()
  (make-instance 'gmpl-language
                 :union-symbol "union"
                 :intersection-symbol "inter"
                 :assignment-symbol "="
                 :difference-symbol "diff"
                 :cartesian-product-symbol "cross"))

(defparameter gmpl (gmpl-language))

(gcode is-a-reference gmpl-language ("~a") (name))

(defmethod generate-code((node index-at)
                         (lang gmpl-language)
                         (stream t))

  (let* ((indexes-code (mapcar (lambda (x) (generate-code x lang nil))
                               (indexes node))))
    (format stream "~a[~{~a~^, ~}]"
            (gcodenil var-name)
            indexes-code)))

(gcode for-all-quantifier gmpl-language ("~a in ~a") (var-name set-name))

(gcode var-in-set gmpl-language ("~a in ~a") (var-name set-name))

(defmethod generate-code ((node var-from-to)
                          (lang gmpl-language)
                          stream)
  (error "In GMPL we can't generate the code (yet) for var-from-to nodes :-(."))

(defmethod generate-code ((node sum-node)
                          (lang gmpl-language)
                          stream)
  (format stream "sum {~{~a~^, ~}} ~a"
          (mapcar (lambda (x) (gcodenil-exp x))
                  (sum-bounds node))
              (gcodenil elements)))

(node-ends-with-separator constraint-node gmpl-language)
(gcode constraint-node gmpl-language
       ("s.t. ~a~a: ~a")
       (id (if (quantifiers node)
                    (format nil " {~{~a~^, ~}}"
                            (mapcar (lambda (x)
                                      (gcodenil-exp x))
                                    (quantifiers node)))
                    "")
           func))

(gcode continuous-variable-type gmpl-language (""))

(gcode integer-variable-type gmpl-language (", integer"))

(gcode binary-variable-type gmpl-language (", binary"))

(node-ends-with-separator variable-declaration-node gmpl-language)
(gcode variable-declaration-node gmpl-language
       ("var ~a~a~a~a~a")
       ((gcodenil-exp (write-name node lang nil))
        (if (domain node)
            (let* ((var-name-generator
                    (make-dummy-variable-name-generator t)))
              (format nil " {~{~a~^, ~}}"
                      (mapcar (lambda (x)
                                (concatenate
                                 'string
                                 (string-downcase
                                  (funcall var-name-generator))
                                 " in "
                                 (generate-code x lang nil)))
                              (domain node))))
            "")
        var-type
        (if (and (lower-bound node))
            (format nil ", >= ~a"
                    (gcodenil lower-bound))
            "")
        (if (and (upper-bound node)
                 (/= (upper-bound node) 0))
            (format nil ", <= ~a"
                    (gcodenil upper-bound))
            "")))

(node-ends-with-separator objective-function-node gmpl-language)
(gcode objective-function-node gmpl-language
       ("~a ~a: ~a")
       (obj id func))

(defmethod generate-code ((node problem-definition-node)
                          (lang gmpl-language)
                          stream)
  (let* (data-section
         elements-without-data
         instructions-without-data)
    ;; first we detect if there is a data section
    (loop for elt in (elements (instr node))
          do (progn
               (if (is-a-data-section elt)
                   ;; then
                   (setf data-section elt)
                   ;; else
                   (push elt elements-without-data))))

    ;; let's reverse the data
    (setf instructions-without-data
          (instructions-list
           (reverse elements-without-data)))

    (format stream "~a"
            (with-output-to-string (s)
              (format s "/*Problem ~a*/~3%"
                      (gcodenil name))
              (format s "~a~2%"
                      (gcodenil-exp instructions-without-data))

              (if data-section
                  (format s "~a~%"
                          (gcodenil-exp data-section )))

              (format s "end;~%")))))

(node-ends-with-separator parameter-declaration-node gmpl-language)
(gcode parameter-declaration-node gmpl-language
       ("param ~a~a~a")
       (name
        (if (domain node)
            (let* ((var-name-generator
                    (make-dummy-variable-name-generator t)))
              (format nil " {~{~a~^, ~}}"
                      (mapcar (lambda (x)
                                (concatenate 'string
                                             (string-downcase
                                              (funcall var-name-generator))
                                             " in "
                                             (generate-code x lang nil)))
                              (domain node))))
            "")
        (cond
          ((null (value node))
           (format nil ""))
          ((listp (value node))
           (format nil " := ~{~a~^, ~}"
                   (mapcar (lambda (x)
                             (generate-code x lang nil))
                           (value node))))
          (t (format nil " := ~a"
                     (generate-code (value node) lang nil))))))

(gcode range-node gmpl-language
       ("~a..~a~a")
       (min-value
        max-value
        (if (and (numberp (increment node))
                 (/= (increment node) 1))
            (format nil " by ~a"
                    (gcodenil increment))
            "")))

(node-ends-with-separator set-declaration-node gmpl-language)
(gcode set-declaration-node gmpl-language
       ("set ~a~a~a")
       ((gcodenil-exp (write-name node lang nil))
        (if (dimension node)
            (format nil " dimen ~a"
                    (gcodenil dimension))
            "")
        (if (value node)
            (cond
              ((subtypep (class-of (first (value node)))
                         'range-node)
               (format nil " := ~a"
                       (generate-code (first (value node)) lang nil)))
              (t (format nil " := {~{~a~^,~}}"
                         (mapcar (lambda (x)
                                   (gcodenil-exp x))
                                 (value node))))
              )
            "")))

(node-ends-with-separator display-node gmpl-language)
(gcode display-node gmpl-language
       ("display ~a") (element))

(node-ends-with-separator solve-node gmpl-language)
(gcode solve-node gmpl-language
       ("solve") ())

(defmethod generate-code :before ((node has-doc)
                                  (lang gmpl-language)
                                  stream)
  (if (doc node)
      (format stream "/* ~a */~%" (doc node))))

(defmethod format-value-of ((node set-value-of-set)
                            (value list)
                            (lang gmpl-language)
                            (data-reader standard-data-reader)
                            stream)
  (format stream "~a"
            (with-output-to-string (s)
              (format s "~{~a~^, ~}"
                      (mapcar (lambda (x) (gcodenil-exp x))
                              value)))))

(defmethod format-value-of ((node set-value-of-set)
                            (value range-node)
                            (lang gmpl-language)
                            (data-reader standard-data-reader)
                            stream)
  (format stream "~a" (gcodenil-exp value)))

(defmethod format-value-of ((node set-value-of-param)
                            (value number)
                            (lang gmpl-language)
                            (data-reader standard-data-reader)
                            stream)
  (format stream "~a" (gcodenil-exp value)))

(defmethod format-value-of ((node set-value-of-param)
                            (value list)
                            (lang gmpl-language)
                            (data-reader standard-data-reader)
                            stream)


  (let* ((param-ref (param-name node))
         (param-decl (original-declaration param-ref))
         (current-domain (domain param-decl))
         ;; let's compute the length of `param name :='
         (name-length (length (name param-ref)))
         (indentation (cl:+ 9 name-length))
         (indentation-str (make-string indentation
                                       :initial-element #\SPACE)))
    ;; from here on we differentiate according the dimension of
    ;; the domain

    (cond
      ;; dimension 1
      ((cl:= (length current-domain) 1)
       (let* ((domain-value (current-value (first current-domain)))
              (max-name-length (loop for e in domain-value
                                     maximizing (length (name e))))
              (column-for-value max-name-length))
         ;; check that the value and the domain have the same
         ;; cardinality

         (if (cl:/= (length domain-value)
                    (length value))
             (error "Error in set-value-of-param: Parameter value and domain set should have the same number of elements."))
         ;; everything is ok, so
         ;; let's print the first two elements:
         (format stream "~a"
                 (with-output-to-string (s)
                   (format s "~a~vT~a"
                           (gcodenil-exp (first domain-value))
                           (1+ column-for-value)
                           (gcodenil-exp (first value))
                           ;; debug
                           ;; (1+ column-for-value)
                           ;; max-name-length
                           )
                   (loop for index-value in (rest domain-value)
                         for param-value in (rest value)
                         do (format s "~%")
                         do (format s "~a ~a~vT~a"
                                    indentation-str
                                    (gcodenil-exp index-value)
                                    (cl:+ column-for-value
                                          indentation
                                          ;; add the whitespaces? :-/
                                          2)
                                    (gcodenil-exp param-value)
                                    ;;debug
                                    ;;(cl:+ column-for-value
                                    ;;       indentation
                                    ;;       2)
                                    ))))))
      ;; dimension 2
      ;; TODO: give space according to size of the
      ;; numbers in the data.
      ((cl:= (length current-domain) 2)
       (let* ((domain-value-1 (current-value (first current-domain)))
              (domain-value-2 (current-value (second current-domain)))
              (max-name-length-in-set-1
               (loop for e in domain-value-1
                     maximizing (length (name e))))
              (column-for-first-column
               (cl:+ 2 max-name-length-in-set-1))
              (column-positions
               (make-array (list (length domain-value-2)))))
         ;; check that the value and the domain have the same
         ;; cardinality
         ;; TODO 
         ;; everything is ok, so

         (format stream "~a"
                 (with-output-to-string (s)
                   ;; the position of the first value is
                   ;; the column for the first column
                   (setf (aref column-positions 0)
                         (cl:+ column-for-first-column
                               indentation))

                   ;; let's print the names of the second set 
                   (format s "~a~a"
                           (make-string column-for-first-column
                                        :initial-element #\SPACE)
                           (gcodenil-exp (first domain-value-2)))

                   ;; add the names of the second set
                   ;; and compute the columns positions
                   (loop for e in (rest domain-value-2)
                         for l in domain-value-2
                         for i from 1
                         do (format s " ~a"
                                    (gcodenil-exp e))
                         do (setf (aref column-positions i)
                                  (cl:+ (aref column-positions (1- i))
                                        (length l)
                                        1)))
                   ;; finally let's add the symbol := and the newline
                   (format s " :=")

                   ;; now let's print each line
                   (loop for set-value1 in domain-value-1
                         for i from 0
                         for row in value
                         do (format s "~%")
                         do (format s "~a~a"
                                    indentation-str
                                    (gcodenil-exp set-value1))
                         ;; now let's add the data
                         do (loop for elt in row
                                  for p from 0
                                  do (format s "~vT~a"
                                             (aref column-positions p)
                                             elt)))

                   ;; (loop for index-value in (rest domain-value)
                   ;;       for param-value in (rest value)
                   ;;       do (format s "~%")
                   ;;       do (format s "~a ~a~vT~a"
                   ;;                  indentation-str
                   ;;                  (gcodenil-exp index-value)
                   ;;                  (cl:+ column-for-value
                   ;;                        indentation
                   ;;                        ;; add the whitespaces? :-/
                   ;;                        2)
                   ;;                  (gcodenil-exp param-value)
                   ;;                  ;;debug
                   ;;                  ;;(cl:+ column-for-value
                   ;;                  ;;       indentation
                   ;;                  ;;       2)
                   ;;                  ))
                   ))))
      )))

(node-ends-with-separator set-value-of-set gmpl-language)

(gcode set-value-of-set gmpl-language ("set ~a := ~a")
       (set-name
        (gcodenil-exp
         (format-value-of node
                          (value node)
                          lang
                          (data-reader node)
                          nil))))

(node-ends-with-separator set-value-of-param gmpl-language)

(defmethod generate-code ((node set-value-of-param)
                          (lang gmpl-language)
                          stream)

  (let* ((param (param-name node))
         (current-domain (domain param))
         (dimension (length current-domain)))

    (format stream "param ~a ~a ~a"
            (gcodenil param-name)
            (if (cl:< dimension 2) ":=" ":")
            (format-value-of node
                             (value node)
                             lang
                             (data-reader node)
                             nil))))

(gcode data-section-node gmpl-language
       ("data;~2%~a~%")
       (instr))

(defun format-params-values-as-list (param value lang stream)
  "This function sets the param value for a give parameter in GMPL."
  (let* ((current-domain (domain param))
         ;; let's compute the length of `param name :='
         (name-length (length (name param)))
         (indentation (cl:+ 9 name-length))
         (indentation-str (make-string indentation
                                       :initial-element #\SPACE)))

    (let* ((domain-value (value (first current-domain)))
           (max-name-length (loop for e in domain-value
                                  maximizing (length (name e))))
           (column-for-value max-name-length))
      ;; check that the value and the domain have the same
      ;; cardinality
      (if (/= (length domain-value)
              (length value))
          (error "Error in set-value-of-param: Parameter value and domain set should have the same number of elements."))
      ;; everything is ok, so
      ;; let's print the first two elements:
      (format stream "~a"
              (with-output-to-string (s)
                (format s "~a~vT~a, ~a"
                        (gcodenil-exp (first domain-value))
                        (1+ column-for-value)
                        (gcodenil-exp (first value))
                        ;; debug
                        (1+ column-for-value)
                        ;; max-name-length
                        )
                (loop for index-value in (rest domain-value)
                      for param-value in (rest value)
                      do (format s "~%")
                      do (format s "~a ~a~vT~a, ~a"
                                 indentation-str
                                 (gcodenil-exp index-value)
                                 (cl:+ column-for-value
                                       indentation
                                       ;; add the whitespaces? :-/
                                       2)
                                 (gcodenil-exp param-value)
                                 ;;debug
                                 (cl:+ column-for-value
                                       indentation
                                       2)
                                 )))))))
