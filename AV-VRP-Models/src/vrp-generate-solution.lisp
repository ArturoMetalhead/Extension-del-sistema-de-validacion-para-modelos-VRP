(in-package :vrp)

(defgeneric generate-solution-by-condition (characteristic
					    high-level-strategy
					    problem-solution
						factibility
					    stream))

(defmethod generate-solution-by-condition (characteristic
					    high-level-strategy
					    problem-solution
						factibility
					    stream)
    (format stream "IN Main method~%"))

(defmethod generate-solution-by-condition :after
					    ((characteristic visit-each-client-at-least-once)
					  (high-level-strategy basic-strategy)
					   problem-solution
					   factibility
					   stream)
    (format stream "IN visit-each-client-at-least-once~%")

    (if (not (clients problem-solution))
	    (setf (clients problem-solution) (make-clients)))

    (format stream "clients: ~a~%" (clients problem-solution))

    (if (not (routes problem-solution))
	    (setf (routes problem-solution) (make-routes (clients problem-solution))))

    (format stream "routes: ~a~%" (routes problem-solution))


)

(defmethod generate-solution-by-condition :after
					    ((characteristic visit-client-at-most-once)
					     (high-level-strategy basic-strategy)
					     problem-solution
						 factibility
					     stream)
    (format stream "IN  visit-client-at-most-once~%")

    (generate-solution-by-condition (make-instance 'visit-each-client-at-least-once) high-level-strategy problem-solution factibility stream)

)

(defmethod generate-solution-by-condition :after
					    ((characteristic begin-in-depot)
					(high-level-strategy basic-strategy)
					problem-solution
					factibility
					stream)

   (format stream "IN  begin-in-depot~%")

    (if (not (routes problem-solution))
	(generate-solution-by-condition (make-instance 'visit-each-client-at-least-once) high-level-strategy problem-solution factibility stream))


   (setf (routes problem-solution) (loop for r in (routes problem-solution) 
				      collecting (push 0 r)))

   (format stream "routes begin in depot: ~a~%" (routes problem-solution)))

(defmethod generate-solution-by-condition :after
					    ((characteristic end-in-depot)
					(high-level-strategy basic-strategy)
					problem-solution
					factibility
					stream)

   (format stream "IN end-in-depot~%")

    (if (not (routes problem-solution))
	(generate-solution-by-condition (make-instance 'visit-each-client-at-least-once) high-level-strategy problem-solution factibility stream))


   (setf (routes problem-solution) (loop for r in (routes problem-solution) 
				      collecting (append r '(0))))

   (format stream "routes end in depot: ~a~%" (routes problem-solution)))

(defmethod generate-solution-by-condition :after
					    ((characteristic dont-overload-vehicle)
					     (high-level-strategy basic-strategy)
					      problem-solution
						  factibility
					      stream)

  (format stream "IN dont-overload-vehicle~%")

  (if (not (routes problem-solution))
      (generate-solution-by-condition (make-instance 'visit-each-client-at-least-once) high-level-strategy problem-solution factibility stream))


   (setf (demand problem-solution) (loop for i in (clients problem-solution)
				       collecting (random 100 1)))
   (setf (demand problem-solution) (push 0 (demand problem-solution)))

   (format stream "demands: ~a~%" (demand problem-solution))

  (let* ((values (loop for r in (routes problem-solution)
		      collecting (loop for i in r
			      sum (nth i (demand problem-solution))))))

    (format stream "values: ~a~%" values)
    (format stream "max value: ~a~%" (eval `(max ,@values)))

    (setf (capacity problem-solution) (random (+(eval `(max ,@values)) 20) (eval `(max ,@values))))

    (format stream "capacity: ~a~%" (capacity problem-solution))))

(defmethod generate-solution-by-condition :after
					    ((characteristic dont-visit-each-client-at-least-once)
					(high-level-strategy basic-strategy)
					problem-solution
					factibility
					    stream)

   (format stream "IN dont-visit-each-client-at-least-once~%")

   (if (not (clients problem-solution))
	   (setf (clients problem-solution) (make-clients)))

   (setf (first factibility) :infeasible);;;;;

   (format stream "clients: ~a~%" (clients problem-solution))

   (if (not (routes problem-solution))
	   (let*  ((n (random (length (clients problem-solution)) 1))
		   (taken-clients (nthcar n (permutation (clients problem-solution)))))
	      (setf (routes problem-solution) (make-routes taken-clients))))

   (format stream "routes: ~a~%" (routes problem-solution)))

(defmethod generate-solution-by-condition :after
					    ((characteristic visit-client-more-than-once)
					(high-level-strategy basic-strategy)
					problem-solution
					factibility
					    stream)

     (format stream "IN visit-client-more-than-once~%")

     (if (not (routes problem-solution))
	      (generate-solution-by-condition (make-instance 'visit-each-client-at-least-once) high-level-strategy problem-solution factibility stream))

     (setf (first factibility) :infeasible);;;;;

     (let* ((actual-routes (routes problem-solution))
	    (rep-client (random-choice (random-choice actual-routes)))
	    (route-to-insert-index (random (length actual-routes)))
	    (route-to-insert (nth route-to-insert-index actual-routes))
	    (new-route (permutation (push rep-client route-to-insert))))

	  (setf (routes problem-solution) (append (nthcar route-to-insert-index actual-routes) (list new-route) (nthcdr (1+ route-to-insert-index) actual-routes) ))

	  (format stream "repeated-client: ~a~%" rep-client)
       )

     (format stream "routes-repeated-client: ~a~%" (routes problem-solution))
  )

(defmethod generate-solution-by-condition :after
					    ((characteristic begin-anywhere)
					(high-level-strategy basic-strategy)
					problem-solution
					factibility
					     stream)

   (format stream "IN begin-anywhere~%")

  (if (not (routes problem-solution))
      (generate-solution-by-condition (make-instance 'visit-each-client-at-least-once) high-level-strategy problem-solution factibility stream))

  (setf (first factibility) :infeasible);;;;;
  (setf (second factibility) :begin-anywhere);;;;;

  (let* ((n (random (length (routes problem-solution)) 1))
	 (indexes (loop for i from 0 to n collecting (random (length (routes problem-solution)))))
	 (new-routes (loop for r in (routes problem-solution)
			   for i from 0 to (length (routes problem-solution))
			   collecting (if (member i indexes)
					  r
					  (push 0 r)))))
      (setf (routes problem-solution) new-routes))

 (format stream "routes that begin anywhere: ~a~%" (routes problem-solution)))

(defmethod generate-solution-by-condition :after
					    ((characteristic end-anywhere)
					(high-level-strategy basic-strategy)
					problem-solution
					factibility
					    stream )

   (format stream "IN  end-anywhere~%")

  (if (not (routes problem-solution))
      (generate-solution-by-condition (make-instance 'visit-each-client-at-least-once) high-level-strategy problem-solution factibility stream))

  ;(setf (first factibility) :infeasible);;;;;

    ;
  (if (and (eq (first factibility) :infeasible)
         (not (null (second factibility)))
         (eq (second factibility) :begin-anywhere))
  (progn

	(let* ((n (random (length (routes problem-solution)) 1))
		(indexes (loop for i from 0 to n collecting (random (length (routes problem-solution)))))
		(new-routes (loop for r in (routes problem-solution)
				for i from 0 to (length (routes problem-solution))
				collecting (if (member i indexes)
						(append r (list (first r)))
						(append r (list (first r)))
						))))
		(setf (routes problem-solution) new-routes))

   ;(append r '(0))

  )
  (progn

  (let* ((n (random (length (routes problem-solution)) 1))
	 (indexes (loop for i from 0 to n collecting (random (length (routes problem-solution)))))
	 (new-routes (loop for r in (routes problem-solution)
			   for i from 0 to (length (routes problem-solution))
			   collecting (if (member i indexes)
					  r
					  (append r '(0))))))
      (setf (routes problem-solution) new-routes))
  
  
  )
  
  )
 (format stream "routes that end anywhere: ~a~%" (routes problem-solution)))

(defmethod generate-solution-by-condition :after
					    ((characteristic overload-vehicles)
					(high-level-strategy basic-strategy)
					problem-solution
					factibility
					    stream )
      (format stream "IN overload-vehicle~%")

  (if (not (routes problem-solution))
      (generate-solution-by-condition (make-instance 'visit-each-client-at-least-once) high-level-strategy problem-solution factibility stream))


   (setf (first factibility) :infeasible);;;

   (setf (demand problem-solution) (loop for i in (clients problem-solution)
				       collecting (random 100 1)))
   (setf (demand problem-solution) (push 0 (demand problem-solution)))

   (format stream "demands: ~a~%" (demand problem-solution))

   (let* ((values (loop for r in (routes problem-solution)
		      collecting (loop for i in r
			      sum (nth i (demand problem-solution))))))

    (format stream "values: ~a~%" values)
    (format stream "max value: ~a~%" (eval `(max ,@values)))

    (setf (capacity problem-solution) (random (1- (eval `(max ,@values))) ))

    (format stream "capacity: ~a~%" (capacity problem-solution))))
