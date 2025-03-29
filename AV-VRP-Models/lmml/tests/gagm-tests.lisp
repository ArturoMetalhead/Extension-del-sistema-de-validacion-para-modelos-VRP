(let* ((s1 '(1 2 3 4))
       (s2 '(2 5 7 1)))
  (format t "set-difference: ~a and ~a:~%   ~a~%"
          s1 s2 (ordered-set-difference s1 s2)))

(let* ((s1 '(1 2 3 4))
       (s2 '(2 7 5 1)))
  (format t "union: ~a and ~a:~%   ~a~%"
          s1 s2 (ordered-union s1 s2)))

(format t "make-keyword: ~a~%" (make-keyword ":~a" 'hello))

(let* ((list '(1 (2 3) (4 (5 6 (7 8 9)) 10))))
  (format t "flattening list ~a:~%~a~%"
          list (flatten-list list)))

(let* ((list '(1 2 3 4 5 6 7 8)))
  (format t "second-elements from list ~a:~%~a~%"
          list (second-elements list)))





(let* ((dict nil))
  (setf dict (insert-class-data dict 'person () '(name :accessor name) '(age)))
  (format t "dict after person: ~a~%" dict))

(let* ((dict nil))
  (setf dict (insert-class-data dict 'person () '(name :accessor name) '(age)))
  (format t "dict after person: ~a~%" dict)
  (setf dict (remove-class-data 'person dict))
  (format t "dict after remove data: ~a~%" dict))

(let* ((dict nil)
       (class-data))
  (setf dict (insert-class-data dict 'person () '(name :accessor name) '(age)))
  (format t "dict after person: ~a~%" dict)
  (setf class-data (get-data-from-dictionary 'person dict))
  (format t "class-data after remove data: ~a~%" class-data))

(let* ((dict nil)
       (class-slots))
  (setf dict (insert-class-data dict 'person () '(name :accessor name) '(age)))
  (format t "dict after person: ~a~%" dict)
  (setf class-slots (get-slots-from-class 'person dict))
  (format t "slots for class person: ~a~%" class-slots))

(let* ((dict nil)
       (class-slots))
  (setf dict (insert-class-data dict 'person ()
                                '(name :accessor name)
                                '(age)))
  (format t "dict after person: ~a~%" dict)
  (setf class-slots (get-slots-from-class 'person dict))
  (format t "slots for class person: ~a~%" class-slots)
  (setf dict (insert-class-data dict 'worker '(person)
                                '(job)
                                '(salary)))
  (format t "dict after person: ~a~%" dict))

(let* ((dict nil)
       (class-slots))
  (setf dict (insert-class-data dict 'person () '(name :accessor name) '(age)))
  (format t "dict after person: ~a~%" dict)
  (setf class-slots (get-slots-from-class 'person dict))
  (format t "slots for class person: ~a~%" class-slots))

(let* ((dict nil)
       (class-slots))
  (setf dict (insert-class-data dict 'person ()
                                '(name :accessor name)
                                '(age)))
  (format t "dict after person: ~a~%" dict)
  (setf class-slots (get-slots-from-class 'person dict))
  (format t "slots for class person: ~a~%" class-slots)
  (setf dict (insert-class-data dict 'worker '(person)
                                '(job)
                                '(salary)))
  (format t "dict after worker: ~a~%" dict)
  (format t "slots for worker: ~a~%"
          (get-all-slots-from-class 'worker dict)))

(let* ((dict nil)
       (class-parents))
  (setf dict (insert-class-data dict 'person ()
                                '(name :accessor name)
                                '(age)))
  (setf class-parents (class-inherit 'person dict))
  (format t "parents for class person: ~a~%" class-parents)
  (setf dict (insert-class-data dict 'worker '(person)
                                '(job)
                                '(salary)))
  (format t "parents for class worker: ~a~%"
          (class-inherit 'worker dict))
  (setf dict (insert-class-data dict 'driver '()
                                '(car)))
  (setf dict (insert-class-data dict 'boss-driver '(worker driver)
                                '()))
  (format t "parents for class boss-driver: ~a~%"
          (class-inherit 'boss-driver dict)))

(let* ((dict nil)
       (class-slots))
  (setf dict (insert-class-data dict 'person () '(name :accessor name) '(age)))
  (setf class-slots (get-all-slots-from-inherit '(person) dict))
  (format t "slots for class person: ~a~%" class-slots))

(let* ((dict nil)
       (class-parents))
  (setf dict (insert-class-data dict 'person ()
                                '(name :accessor name)
                                '(age)))
  (setf class-parents (class-inherit 'person dict))
  (setf dict (insert-class-data dict 'worker '(person)
                                '(job)
                                '(salary)))
  (setf dict (insert-class-data dict 'driver '()
                                '(car)))
  (setf dict (insert-class-data dict 'boss-driver '(worker driver)
                                '()))
  (format t "all slots for boss-driver: ~a~%"
          (get-all-slots-from-inherit '(person worker driver) dict)))

(let* ((dict nil)
       (class-slots))
  (setf dict (insert-class-data dict 'person ()
                                '((name :accessor name) (age))))
  (setf class-slots (get-all-slots-from-inherit
                     '(person) dict))
  (format t "slots for class person: ~a~%" class-slots))

(let* ()
  (clear-properties-dict)
  (format t "slots dict: ~a~%" slots-dict)
  (format t "initarg dict: ~a~%" initarg-dict)
  (format t "accessor dict: ~a~%" accessor-dict))

(create-class-data person
                   "A class to represent a person."
                   ()
                   ((name :accessor name :initarg :name)
                    (age  :accessor age  :initarg :age))
                   (defun person (name age)
                     (make-instance 'person
                                    :name name
                                    :age age))
                   ("~a is ~a years old." name age))

(gcodenil name)

(gcodenil-exp (+ 1 (age node)))

(add-new-patterns newline (format stream "~%"))

(gif name)

(gif name :then-code (format stream "Mr ~a" (gcodenil name)))

(print (make-gcodenil-list `(name age job)))

(gformat t "My name is ~a and I am ~a years old."
         name age)





(gcode person natural-language
               ("My name is ~a and I am ~a years old.")
               (name age))

(let* ((dict nil)
       (class-data))
  (setf dict (insert-class-data dict 'person () '(name :accessor name) '(age)))
  (format t "dict after person: ~a~%" dict)

  (setf class-data (get-data-from-dictionary 'person dict))
  (format t "class-data after remove data: ~a~%" class-data))

(let* ()
  (format t "~%Testing print-object keys:~%")

  (update-left-key "<")
  (format t "Left key for print-object: ~a (expect >)~%"
          print-object-left-key)

  (format t "Right key for print-object: ~a~%"
          print-object-right-key)
  (update-right-key ">")
  (format t "Rigth key for print-object: ~a (expect >)~%"
          print-object-right-key)


  (update-key "[" "]")
  (format t "Left key for print-object: ~a (expect [)~%"
          print-object-left-key)
  (format t "Right key for print-object: ~a (expect ])~%"
          print-object-right-key)

  (set-print-object-keys-to-default)
  (format t "Left key for print-object: ~a (expect ~a)~%"
          print-object-left-key
          *default-print-object-left-key*)
  (format t "Right key for print-object: ~a (expect ~a)~%"
          print-object-right-key
          *default-print-object-right-key*)

  )

(let* ()
    (format t "Standard-print-object for a person with name and age:
~a~%"
            (standard-print-object 'person '(name age) " " "~a")))



(format t "'((int a) (double b)): ~a~%"
        (remove-type-to-type-parameters '((int a) (double b))))





(let* ((test-data '((1 2 3 4) (5 6) (7) 8)))
  (format t "first element in ~a:~%~17t~a~%"
          test-data
          (only-first-element-in-list test-data)))




