(in-package :vrp)

(defclass visit-client-at-most-once () ())

(defclass dont-overload-vehicle () ())

(defclass begin-in-depot () ())

(defclass end-in-depot () ())

(defclass visit-each-client-at-least-once () ())

(defclass visit-client-more-than-once () ()
(:documentation "With this constraint the 'visit customer only once' is broken adding a random customer in two routes (may be the same)."))

(defclass overload-vehicles () ()
(:documentation "Breaks the 'don't overload the vehicles' constraint by choosing the minimum of the load of each vehicle minus one."))

(defclass begin-anywhere () ()
(:documentation "The routes may begin customer place, i.e. they aren't forced to begin in the depot."))

  (defclass end-anywhere () ()
(:documentation "The routes may end customer place, i.e. they aren't forced to end in the depot."))

(defclass dont-visit-each-client-at-least-once () ())

(defgeneric opposite-characteristics (class))

(defmacro def-opposite (class &rest opposite-classes)
	`(defmethod opposite-characteristics ((class (eql ',class)))
	  ',opposite-classes))

(def-opposite visit-client-at-most-once visit-client-more-than-once)

(def-opposite dont-overload-vehicle overload-vehicles)

(def-opposite begin-in-depot begin-anywhere)

 (def-opposite end-in-depot end-anywhere)

(def-opposite visit-each-client-at-least-once dont-visit-each-client-at-least-once)
