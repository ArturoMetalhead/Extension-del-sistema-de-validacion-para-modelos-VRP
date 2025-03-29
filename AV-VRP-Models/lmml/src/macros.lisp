(in-package :lmml)

(defmacro make-binary-operations (list-with-definitions)
  `(progn
     ,@(loop for (name operation-doc op) in list-with-definitions
             for actual-name = (symb name '-op)
             for print-obj-string = (format nil "(~a ~~a ~~a)" op)
             collect `(progn
                        (defnode ,actual-name (binary-operation)
                          ()
                          :lambda-list (left right)
                          :string-obj (,print-obj-string left right)
                          :documentation ,(format nil "Class representing the ~a operation" operation-doc))))))

(defmacro make-binary-operations-layer-2-names (list-with-definitions)
  `(progn
     ,@(loop for (class-name operation) in list-with-definitions
             for actual-name = (symb class-name '-op)
             when operation
             collect `(defun ,operation (left right)
                          (,actual-name left right)))))

(defmacro generate-code-for-binary-operators
    (language format-string list-with-order list-with-operations)
  (let* ((left `(gcodenil left))
         (right `(gcodenil right))
         (defmethods-definitions
          (loop for (op symbol description)
                in (if (symbolp list-with-operations)
                       (symbol-value list-with-operations)
                       list-with-operations)
                for node-name = (symb op '-op)
                for node-symbol = (symb op "-symbol")
                for operator = `(generate-code (,node-symbol lang)
                                               lang nil)
                for actual-order = (loop for elt in list-with-order
                                         collecting (cond
                                                      ((eq elt 'op)
                                                       operator)
                                                      ((eq elt 'left) left)
                                                      ((eq elt 'right) right)))
                for docstring = (format nil "Generate infix code for node ~a in language ~a."
                                        node-name
                                        language)
                collecting `(defmethod generate-code ((node ,node-name)
                                                      (lang ,language)
                                                      stream)
                              ,docstring
                              (format stream ,format-string ,@actual-order)
                              ))))
    ;; (format stream ,format-string ,@actual-order))))))
    `(progn
       ,@defmethods-definitions)))

(defmacro generate-value-for-binary-operators
    (list-with-data)
  (let* ((left `(value (left obj)))
         (right `(value (right obj)))
         (method-declarations
          (loop for (class op) in list-with-data
                collecting `(defmethod value
                                ((obj ,(symb class '-op)))
                              (,op ,left ,right)))))

    `(progn
       ,@method-declarations)))

(defmacro generate-current-value-for-binary-operators
    (list-with-data)
  (let* ((left `(current-value (left obj)))
         (right `(current-value (right obj)))
         (method-declarations
          (loop for (class op) in list-with-data
                collecting `(defmethod current-value
                                ((obj ,(symb class '-op)))
                              (,op ,left ,right)))))

    `(progn
       ,@method-declarations)))
