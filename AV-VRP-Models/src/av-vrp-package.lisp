(defpackage :av-vrp
  (:shadowing-import-from :lmml :set + / - * >= <= = < >)
  (:import-from :lmml :mkstr
		      :symb
		      :make-keyword
		      :bformat
		      :for-all-quantifier
		      :sumf
		      :variable-declaration
		      :problem
		      :parameter
		      :constraint
		      :minimize
		      :maximize
		      :continuous-variable
		      :integer-variable
		      :binary-variable
		      :lisp
		      :*my-var-param-set-list*
		      :gmpl)

       (:import-from :vrp
		     ;;  vrp charactristics
			  :visit-client-at-most-once
			  :dont-overload-vehicle
			  :begin-in-depot
			  :begin-in-depot
			  :visit-each-client-at-least-once

		      ;; define vrp and current high level strategy    
			  :def-vrp-description
			  :basic-strategy

		      ;; sets, variables and parameters
			  :number-vehicles
			  :capacity-vehicles
			  :vehicle-goes-from-client-i-to-j
			  :demand-each-client
			  :vehicles-charge-after-client-i
			  :vehicles-charge-residual-from-client-i-to-j
			  :client-set
			  :client-depot-set
			  :number-clients

		       ;; model evaluator
			  :model-evaluator

			  )
   (:use :cl-user :common-lisp :gagm))
