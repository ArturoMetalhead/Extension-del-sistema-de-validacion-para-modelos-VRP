(in-package vrp)

(defgeneric meaning-restriction (problem-solution variable-parameter-meaning variable-parameter stream))

(defmethod meaning-restriction (problem-solution variable-parameter-meaning variable-parameter stream)
    (format stream "IN main meaning-restriction~%"))

(defmethod meaning-restriction :after (problem-solution
                        (variable-parameter-meaning vehicle-goes-from-client-i-to-j-meaning)
				      variable-parameter 
				      stream)
	(format stream "IN vehicle-goes-from-client-i-to-j MEANING~%")
	

	;implement the meaning-restriction here

)


	(defmethod meaning-restriction :after (problem-solution
	                    (variable-parameter-meaning vehicles-charge-residual-from-client-i-to-j-meaning)
				      variable-parameter
				      stream)


		(format stream "IN vehicles-charge-residual-from-client-i-to-j  MEANING~%")
		(let ((n (+ 2 (length (clients problem-solution)))))
		(setf (x-ij variable-parameter) (make-array (list n n) :initial-element 0))

        (let ((original-routes (routes problem-solution))  
              (new-routes nil))

        (block outer-loop
		 (loop
          ;generating new routes
		 (setf new-routes (make-routes (clients problem-solution)))

		 ;(format stream "clients: ~a~%" (clients problem-solution))

		 (setf new-routes (loop for r in new-routes
				      collecting (push 0 r)))

		 (setf new-routes (loop for r in new-routes 
				      collecting (append r '(0))))
         ;checking if new routes are different from original routes
		(loop for original-sublist in original-routes
			always (not (equal (first new-routes) original-sublist))
			finally (return-from outer-loop))
		 )
		)

		 (format stream "NEW ROUTES: ~a~%" new-routes)
		 (format stream "ORIGINAL ROUTES: ~a~%" original-routes)

		 (loop for r in new-routes
			do (loop for i in r 
				for j in (cdr r)
				do (if (eq j 0)
					(setf (aref (x-ij variable-parameter) i (1- n)) 1)
					(setf (aref (x-ij variable-parameter) i j) 1))
				))
		)
	    )
		(format stream "x_ij: ~a~%" (x-ij variable-parameter))
	)