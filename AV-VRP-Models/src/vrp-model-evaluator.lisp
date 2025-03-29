(in-package :vrp)
(defmacro setf-values (set-param-var-list slots-list  lls-instance)
  (format t "slot-list: ~a~%" slots-list)
     (let* ((temporal-value (mapcar (lambda (x) (eval `(,x ,lls-instance))) slots-list))
	    (setting (mapcar (lambda (x y) `(setf ,x ',y)) set-param-var-list temporal-value)))
		  (format t "temp: ~a~%" setting)
       `(progn ,@setting)

		  )
  )


(defmacro invoke-function (function lls-slots lls stream)
   (let ((args  (append (mapcar (lambda (x) `(,x ,lls)) lls-slots) `(,stream)) ))
      `(,function ,@args)))


(defun pass-test  (function-list args-for-functions lls-instance test-number stream)

  (let ((results (loop for function in function-list
		       for r = (eval `(invoke-function ,function
							    ,args-for-functions
							    ,lls-instance ,stream))
			 do (format stream "~a: ~a~%" function r)
			  ;;  (format t "~a: ~a~%" function r)
			 collect r )))

    (if (eq test-number 1)
	 (loop for i in results
	      always i)


	(not (loop for i in results
	      always i)
	   ))))

(defun sorted-replace (characteristics opposites current-subset)
     (loop for c in characteristics
	   for o in opposites
	   collecting (if (member c current-subset)
			  c
			  o)))

(defun extract-variable-names ( variable-set-meaning )
  (mapcar #'cdr variable-set-meaning))

;;;;;
(defparameter *variables-with-meaning* (list 'vehicles-charge-residual-from-client-i-to-j)
)

(defclass VEHICLES-CHARGE-RESIDUAL-FROM-CLIENT-I-TO-J-MEANING () ())
(defclass VEHICLE-GOES-FROM-CLIENT-I-TO-J-MEANING () ())
;;;;;

(defun generate-code-and-load-file (model symb-meaning)
   ;; set parameter *my-var-param-set-list* to problem's variables, parameters and sets
   (setf *my-var-param-set-list* (append (mapcar (lambda (x) (car x)) symb-meaning) '(stream)))

   ;; generate code (functions) in lisp to evaluate model constraints from LMML model 
   (with-open-file (stream "./src/functions-generated.lisp" :direction :output
							   :if-exists :supersede
							   :if-does-not-exist :create)
   (generate-code model lisp stream))

   ;; LOAD file with functons to evaluate
   (load "./src/functions-generated.lisp"))

(defun model-evaluator (model symb-meaning problem-description high-level-strategy output-file index)


   (generate-code-and-load-file model symb-meaning)

   (let* ((function-list (reverse (generate-code model lisp nil)))
	  (ps-pair-symbol (symb "cvrp-problem-solution")) ;; create a symbol for problem-solution-class
	  (sorted-characteristics (topologic-sort (get-description-elements problem-description)
						  high-level-strategy));; sort characteristics by strategy
	  (sorted-op-characteristics (mapcar (lambda (x) (car (opposite-characteristics x)))
					    sorted-characteristics)) ;; take opposites
	  (bipartition (power-set sorted-characteristics)) ;; Gnerate all subsets for bipartition
	  (current-characteristics nil) 
	  (args-for-functions (mapcar (lambda (x) (get-slot (cdr x))) symb-meaning))
	  (lls-instance (eval `(progn (defclass model-strategy ,(mapcar (lambda (x) (cdr x)) symb-meaning) ())
						(make-instance 'model-strategy)))))

    ;; create problem-solution pair class 
    (eval `(def-problem-solution-pair ,ps-pair-symbol ,high-level-strategy ,problem-description))

      (with-open-file (stream output-file :direction :output
							:if-exists :supersede
							:if-does-not-exist :create)
     (loop for current in bipartition
	   for i from 1 to (length bipartition)
	   do (setf current-characteristics (sorted-replace sorted-characteristics
							    sorted-op-characteristics
							    current))


			;creating meaning tests

			(if (eq i 1)

			    (progn
					;extract variable names from symb-meaning
					(let ((variable-names (extract-variable-names symb-meaning)))  

						(loop for variable in *variables-with-meaning*
						if (member variable variable-names)

						do (progn

							(bformat stream "~%MEANING-TEST:~a ~%" variable)
						
						(let* ((class-name (intern (concatenate 'string (string variable) "-MEANING")))
							(vrp-problem (eval `(progn (defclass ,(symb "class- M"  ) ,current-characteristics ())
										(make-instance ',(symb "class- M" )))))
							(results2 (loop for j from 1 to index
										collect (let ((ps-pair-instance2 (make-instance ps-pair-symbol))(factibility (list "feasible-meaning" nil)))

											;; generate high level solutions
												(format stream "~%Solution: ~a~%" j)
																			;;  (format t "~%Solution: ~a~%" j)
								(Generate-solution-by-condition vrp-problem (make-instance high-level-strategy) ps-pair-instance2 factibility stream)

								(setf old-clients (clients ps-pair-instance2));;;;;;

								(from-high-to-low ps-pair-instance2 lls-instance factibility class-name stream)

								(setf (clients ps-pair-instance2) old-clients);;;;;

								(meaning-restriction ps-pair-instance2 (make-instance class-name) lls-instance stream)

								(pass-test function-list args-for-functions lls-instance "M" stream)
								
								
								))))


							(if (loop for r in results2
								always r)
								(format t "[MEANING-TEST] PASSED~%")
								(format t "[MEANING-TEST] FAILED~%")
							)
						)
						)
						)
					)
				)
			)

			;;;;;;;

		   (bformat stream "TEST: ~a" i)
		   (let* ((class-name "")
			      (vrp-problem (eval `(progn (defclass ,(symb "class-" i ) ,current-characteristics ())
						(make-instance ',(symb "class-" i )))))

			  (results (loop for j from 1 to index
					       collect (let ((ps-pair-instance (make-instance ps-pair-symbol))(factibility (list nil nil)))

							  ;; generate high level solutions
							      (format stream "~%Solution: ~a~%" j)
                                                            ;;  (format t "~%Solution: ~a~%" j)


				(setf (first factibility) :feasible)
				
				(Generate-solution-by-condition vrp-problem (make-instance high-level-strategy) ps-pair-instance factibility stream)

				 
				(from-high-to-low ps-pair-instance lls-instance factibility class-name stream)
				(pass-test function-list args-for-functions lls-instance i stream)))))


			  (if (loop for r in results
			       always r)
				 (format t "[TEST: ~a] PASSED~%" i)
				 (format t "[TEST: ~a] FAILED~%" i)
			      )
		     )


		)
		)))
