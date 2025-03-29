(in-package :vrp)
(defgeneric get-slot (var-param-set))

(defmacro def-variable-parameter (name slot-name)
  `(progn (defclass ,name ()  
      (,(make-slot-by-name slot-name)))

   (defmethod get-slot ((var-param-set (eql ',name)))
       ',slot-name )

  ))

(def-variable-parameter number-vehicles K)

(def-variable-parameter capacity-vehicles C)

(def-variable-parameter vehicle-goes-from-client-i-to-j x-ij)

(def-variable-parameter demand-each-client d-i)

(def-variable-parameter min-vehicles-subset r-S)

(def-variable-parameter vehicles-charge-after-client-i u-i)

(def-variable-parameter vehicles-charge-residual-from-client-i-to-j u-ij)

(def-variable-parameter client-set v-0)

(def-variable-parameter client-depot-set V)

(def-variable-parameter number-clients n)

(defclass cvrp-two-indexes-exp-vehicle-flow (number-vehicles min-vehicles-subset vehicle-goes-from-client-i-to-j) ())

(defclass cvrp-two-indexes-poli-vehicle-flow (number-vehicles vehicle-goes-from-client-i-to-j demand-each-client capacity-vehicles vehicles-charge-after-client-i) ())

(defclass cvrp-commodity-flow (number-vehicles vehicle-goes-from-client-i-to-j demand-each-client capacity-vehicles vehicles-charge-residual-from-client-i-to-j) ())
