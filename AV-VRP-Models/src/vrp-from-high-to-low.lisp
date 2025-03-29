(in-package :vrp)

(defgeneric from-high-to-low (problem-solution  variable-parameter factibility class-name stream))

(defmethod from-high-to-low (problem-solution  variable-parameter factibility class-name stream )
  (format stream "IN main method~%"))



(defmethod from-high-to-low :after (problem-solution  
				   (variable-parameter number-vehicles)
				   factibility
				   class-name
				   stream)

	   (format stream "Factibility: ~a~%" (first factibility))
       (format stream "IN number-vehicles ~%")
       (setf (K variable-parameter) (length (routes problem-solution)))
       (format stream "K: ~a~%" (K variable-parameter))
   )

(defmethod from-high-to-low :after (problem-solution 
				   (variable-parameter capacity-vehicles)
				   factibility
				   class-name
				   stream)
       (format stream "IN number-vehicles ~%")

       (setf (C variable-parameter) (capacity problem-solution))

       (format stream "C: ~a~%" (capacity problem-solution))
  )

(defmethod from-high-to-low :after (problem-solution  
				(variable-parameter vehicle-goes-from-client-i-to-j)
				factibility
				class-name
				 stream) 
    
	
	; (if (and (string= (first factibility) "feasible-meaning")
	;         (string= (symbol-name class-name) "VEHICLES-CHARGE-RESIDUAL-FROM-CLIENT-I-TO-J-MEANING")
	;    )
	;   (meaning-restriction problem-solution (make-instance class-name) variable-parameter stream)
	;   ( progn
			(format stream "IN vehicle-goes-from-client-i-to-j ~%")
			(let ((n (+ 2 (length (clients problem-solution)))))

			(setf (x-ij variable-parameter) (make-array (list n n) :initial-element 0))
			(loop for r in (routes problem-solution)
				do (loop for i in r 
					for j in (cdr r)
					do (if (eq j 0)
						(setf (aref (x-ij variable-parameter) i (1- n)) 1)
						(setf (aref (x-ij variable-parameter) i j) 1))
					)))

			(format stream "x_ij: ~a~%" (x-ij variable-parameter))

	;   ))
  )

(defmethod from-high-to-low :after (problem-solution  
				(variable-parameter demand-each-client)
				factibility
				class-name
				stream ) 
    (format stream "IN demand-each-client~%")
    (let ((n (1+ (length (clients problem-solution)))))

	(setf (d-i variable-parameter) (make-array (list n) :initial-contents (loop for d in (demand problem-solution)
							collect d))))
    (format stream "d_i: ~a~%" (d-i variable-parameter))
  )

(defmethod from-high-to-low :after (problem-solution  
				   (variable-parameter min-vehicles-subset)
				   factibility
				   class-name
				   stream)
  (format stream "IN min-vehicles-subset~%")

  (let* ((p-set (power-set (clients problem-solution)))
	(r (loop for s in p-set
		 collecting (ceiling (loop for c in s sum (nth c (demand problem-solution))) (capacity problem-solution)) )))
	(setf (r-S variable-parameter) (list p-set r)))

  (format stream "r(S): ~a~%" (r-S variable-parameter)))

(defmethod from-high-to-low :after (problem-solution  
				   (variable-parameter vehicles-charge-after-client-i)
				   factibility
				   class-name
				   stream ) 

      (format stream "IN vehicles-charge-after-client-i~%")  
      (let ((n (1+ (length (clients problem-solution))))
	    (demand (demand problem-solution)))

	   (setf (u-i variable-parameter) (make-array (list n) :initial-element 0))
	   (loop for r in (routes problem-solution)
	       do (let ((temp 0))
		       (loop for i in r 
		       do (setf temp (+ temp (nth i demand)))
			  (setf (aref (u-i variable-parameter) i) temp))))
	   (setf (aref (u-i variable-parameter) 0) 0))
     (format stream "u_i: ~a~%" (u-i variable-parameter))
  )

(defmethod from-high-to-low :after (problem-solution  
				   (variable-parameter vehicles-charge-residual-from-client-i-to-j)
				   factibility
				   class-name
				   stream) 
       (format stream "IN vehicles-charge-residual-from-client-i-to-j~%")
	   

	   (let ((n (+ 2 (length (clients problem-solution))))
	    (cap (capacity problem-solution)))

	   (setf (u-ij variable-parameter) (make-array (list n n) :initial-element 0))
	   (loop for r in (routes problem-solution)
	       do (let ((temp (loop for client in r sum(nth client (demand problem-solution) ))))
		       (loop for i in r
			     for j in (cdr r)

		       do (setf temp (- temp (nth i (demand problem-solution))))
			  (if (eq j 0)
			      (progn
				 (setf (aref (u-ij variable-parameter) i (1- n)) temp)
				 (setf (aref (u-ij variable-parameter) (1- n) i) (- cap temp)))
			      (progn
				 (setf (aref (u-ij variable-parameter) i j) temp)
				 (setf (aref (u-ij variable-parameter) j i) (- cap temp)))


			      )))))
	   (format stream "u_ij: ~a~%"(u-ij variable-parameter))
	     
       )

(defmethod from-high-to-low :after (problem-solution  
				   (variable-parameter client-set)
				   factibility
				   class-name
				   stream)
  (format stream "IN client-set~%")

  (setf (v-0 variable-parameter) (clients problem-solution))

  (format stream "V-0: ~a~%" (v-0 variable-parameter)))

(defmethod from-high-to-low :after (problem-solution  
				   (variable-parameter client-depot-set)
				   factibility
				   class-name
				   stream)
  (format stream "IN client-depot-set~%")

  (setf (v variable-parameter) (append (push 0 (clients problem-solution)) (list (length (clients problem-solution)))))

  (format stream "V: ~a~%" (v variable-parameter)))

(defmethod from-high-to-low :after (problem-solution  
				   (variable-parameter number-clients)
				   factibility
				   class-name
				   stream)
  (format stream "IN number-clients~%")

  (setf (n variable-parameter) (length (clients problem-solution)))

  (format stream "n: ~a~%" (n variable-parameter)))
