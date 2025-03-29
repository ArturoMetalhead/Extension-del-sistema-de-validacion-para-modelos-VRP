(in-package :vrp)

(defgeneric get-description-elements (class))

(defmacro def-vrp-description (name characteristics)
        `(progn
	     (defclass ,name ,characteristics ())
	     (defmethod get-description-elements ((class (eql ',name)))
				',characteristics)))

(def-vrp-description cvrp-description (visit-each-client-at-least-once dont-overload-vehicle visit-client-at-most-once begin-in-depot end-in-depot))
