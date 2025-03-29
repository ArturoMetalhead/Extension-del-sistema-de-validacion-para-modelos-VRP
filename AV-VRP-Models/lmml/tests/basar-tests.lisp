(format t "sum node: ~a~%" (add 3 4))

(format t "multiplication node: ~a~%" (mult 3 4))

(format t "assignment node: ~a~%" (assign-to 3 4))

(format t "variable reference: ~a~%" (var-reference "x"))

(let* (x)
  (format t "variable declaration:~%~5t~a~%" (new-var x)))

(format t "print statement: ~a~%" (bprint (add 4 3)))

(format t "~a~%"
        (bprogram
         (new-var x)
         (new-var y)
         (assign-to x 5)
         (assign-to y (mult 3 (add x 5)))
         (bprint y)))

(format t "~a~%"
        (bprogram
         (new-var x)
         (new-var y)
         (assign-to x 5)
         (assign-to y (mult 3 (add x 5)))
         (bprint y)))

(defparameter *basic-language* (basic-language))

(generate-code 4 *basic-language* t)

(generate-code "hello world!" *basic-language* t)

(generate-code 'hello-world *basic-language* t)

(let* ((node (bprogram 3 "hola" 'mundo 4)))
  (format t "Testing the code generation for program:~%")
  (generate-code node *cl* t))

(let* ((node (add 3 4)))
  (generate-code node *cl* t))

(let* ((node (mult 3 4)))
  (generate-code node *cl* t))

(let* ((node (assign-to 3 4)))
  (generate-code node *cl* t))

(let* ((node (var-reference 'x)))
  (generate-code node *cl* t))

(let* (x
       (node (new-var x)))
  (generate-code node *cl* t))

(let* (x
       (node (bprint 3)))
  ;; first we create a variable named x
  (new-var x)
  (terpri)

  (generate-code node *cl* t)
  (terpri)

  (setf node (bprint (add 3 4)))
  (generate-code node *cl* t)
  (terpri)

  (setf node (bprint (add x 4)))
  (generate-code node *cl* t))

(let* ((node (bprogram
              (new-var x)
              (new-var y)
              (assign-to x 5)
              (assign-to y (add 5 (mult 3 x)))
              (bprint y))))
  (format t "Testing the code generation for program:~%")
  (generate-code node *cl* t)

  ;; now we'll write the code to a file
  (with-open-file (f "src/basar-example.lisp"
                     :direction :output
                     :if-exists :supersede)
    (generate-code node *cl* f)))
