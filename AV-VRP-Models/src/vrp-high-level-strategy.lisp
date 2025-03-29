(in-package :vrp)

(defmacro def-high-level-strategy (name prerequisites required-classes)
    (let*   ((class `(defclass ,name () ()))
	    (prerequisites-methods (loop  for p in prerequisites
		collecting (prerequisite-characteristics-code (car p) (cdr p) name)))
	    (required-classes-methods (loop  for r in required-classes
		collecting (required-classes-code (car r) (cdr r) name)) ))

	`(progn ,class
		,@prerequisites-methods
		,@required-classes-methods )))

(let* ((prerequisites '((visit-each-client-at-least-once)
			(dont-overload-vehicle visit-client-at-most-once)
			(begin-in-depot visit-each-client-at-least-once)
			(end-in-depot visit-each-client-at-least-once)
			(visit-client-at-most-once visit-each-client-at-least-once)))

       (has-x '((visit-each-client-at-least-once has-routes has-clients)
		(dont-overload-vehicle has-routes has-clients has-capacity has-demand)
		(begin-in-depot has-routes has-clients)
		(end-in-depot has-routes has-clients)
		(visit-client-at-most-once has-clients has-routes))))

  (eval `(def-high-level-strategy basic-strategy ,prerequisites ,has-x)))

(def-problem-solution-pair cvrp-problem-solution basic-strategy cvrp-description)
