(in-package :lmml)

(defparameter example1
  (problem-node "example1"
                (variable-declaration x :var-type binary-variable)
                (variable-declaration y)

                (maximize "obj" (add-op (mult-op 4 x)
                                    (mult-op 2 y)))

                (constraint-node "r1" (less-or-equal-op (add-op x y) 3))))

(defparameter example2
  (problem-node "example2"
                (variable-declaration x :var-type binary-variable)
                (variable-declaration y)

                (maximize "obj" (+ (* 4 x)
                                   (* 2 y)))

                (constraint-node "r1" (<= (+ x y) 3))))

(DEFPARAMETER example2.5
  (PROBLEM-NODE "example2"
                (variable-declaration x :var-type binary-variable)
                (variable-declaration y)

                (maximize "obj" (+ (* 4 x)
                                   (* 2 y)))

                (constraint "r1" (<= (+ x y) 3))))

(defparameter example3
  (problem-node "example3"
                (parameter c1 :value 4)
                (parameter c2 :value 2)
                (variable-declaration x :var-type binary-variable
                                      )
                (variable-declaration y)

                (maximize "obj" (+ (* c1 x)
                                   (* c2 y)))

                (constraint-node "r1" (<= (+ x y) 3))))

(defparameter example4
  (problem-node "example4"
                (set I :value '(1 2))
                (parameter c1 :value 4)
                (parameter c2 :value 2)
                (variable-declaration x :domain (list I)
                                      :var-type binary-variable)

                (maximize "obj" (+ (* c1 (index-at x 1))
                                   (* c2 (index-at x 2))))

                (constraint-node "r1" (<= (+ (index-at x 1)
                                             (index-at x 2))
                                          3))))

(defparameter example5
  (problem-node "example5"
                (parameter n :value 5)
                (set I :value (list (range 1 (+ n 1))))
                (parameter c1 :value 4)
                (parameter c2 :value 2)
                (variable-declaration x :domain (list I)
                                      :var-type binary-variable)

                (maximize "obj" (+ (* c1 (index-at x 1))
                                   (* c2 (index-at x 2))))

                (constraint-node "r1" (<= (+ (index-at x 1)
                                             (index-at x 2))
                                          n))

                (solve)
                (display x)))

(defparameter example6
  (problem-node "example6"
                (set I :value {1 2})
                (parameter c1 :value 4)
                (parameter c2 :value 2)
                (variable-declaration x :domain {I I}
                                      :var-type binary-variable)

                (maximize "obj" (+ (* c1 (index-at x 1 1))
                                   (* c2 (index-at x 2 1))))

                (constraint-node "r1" (<= (+ (index-at x 1 1)
                                             (index-at x 2 1))
                                          3))))

(defparameter example7
  (problem-node "example7"
                (set I :value {1 2})
                (parameter c1 :value 4)
                (parameter c2 :value 2)
                (variable-declaration x :domain {I I}
                                      :var-type binary-variable)

                (maximize "obj" (+ (* c1 [x 1 1])
                                   (* c2 [x 2 1])))

                (constraint-node "r1" (<= (+ [x 1 1]
                                             [x 2 1])
                                          3))))

(defparameter cvrp1
  (problem-node "cvrp1"
      (parameter n :doc "Number of clients")
      (parameter P :doc "Capacity of the vehicles")
      (parameter K :doc "Number of vehicles")
      (parameter M :doc "Sum of the demands")
      (set V :value (list (range 1 n)) :doc "Set of clients")
      (set VD :value (list (range 0 (+ n 1))) :doc "Clients and depots")

      (parameter c :domain (list V V) :doc "Distance betweens clients")
      (parameter d :domain (list V) :doc "Demand of each client")


      (variable-declaration x
                            :var-type binary-variable
                            :domain (list VD VD)
                            :doc "1 if client i is visited right before client j.")

      (variable-declaration y
                            :domain (list VD VD)
                            :doc "A variable to track the commodity flow")

      (minimize "obj" (sumf ((i in VD))
                            (sumf ((j in VD))
                                  (* (index-at x i j)
                                     (index-at c i j)))))

      (constraint "r1"
                  (= (sumf ((j in VD))
                           (- [y j i] [y i j]))
                     (* 2 [d i]))
                  :quantifiers ((for-all-quantifier i V))
                  :doc "Flow preserving condition")

      (constraint "r2" (= (sumf ((j in V)) [y 0 j])
                          M)
                  :doc "All the demand leaves the depot")

      (constraint "r3" (= (sumf ((j in V)) [y j 0])
                          (- (* K P) M))
                  :doc "The depot receives the remaining demand")

      (constraint "r4" (= (sumf ((j in V)) [y (+ n 1) j])
                          (* K P))
                  :doc "Correct Yij values leaving the depot")

      (constraint "r5" (= (+ [y i j] [y j i])
                          (* P [x i j]))
                  :quantifiers ((for-all-quantifier i VD)
                                (for-all-quantifier j VD))
                  :doc "Relationship between Yij values at each node")

      (constraint "r6" (= (sumf ((j in V))  (+ [x i j]
                                               [x j i]))
                          2)
                  :quantifiers ((for-all-quantifier i V))
                  :doc "Each client is visited and departed exactly once.")

      ))



   ;; (problem "cvrp"
   ;;              (set V)
   ;;              (set I)
   ;;              (param c :domain {V V})
   ;;              (param d :domain {V})
   ;;              (param n)
   ;;              (param P)
   ;;              (param K)
   ;;              (param M)
   ;;              (binary-variable x :domain {V V})
   ;;              (variable        y :domain {V V})
   ;;              (minimize (sum (a in V)
   ;;                          (sum (b in V)
   ;;                              x[a b] * c[a b])))
   ;;              (s.t. (sum (b in V)
   ;;                        y[b a] - y[a b]) = 2 * d[a]
   ;;                    (forall a in I))
   ;;              (s.t. (sum (b in I) y[0 b]) = M)
   ;;              (s.t. (sum (b in I) y[b 0]) = K * P - M)
   ;;              (s.t. (sum (b in I) y[n + 1 b]) = K * P)
   ;;              (s.t. y[a b] + y[b a] = P * x[a b]
   ;;                    (forall a in V)
   ;;                    (forall b in V))
   ;;              (s.t. (sum (b in V)  x[a b] + x[b a]) = 2
   ;;                    (forall a in I)))

(defparameter gmpl-example1
  (problem-node "A transportation problem"

      (set I :doc "Canning plants")

      (set J :doc "Markets")


      (parameter a :domain {I} :doc "Capacity of plant i in cases")

      (parameter b :domain {J} :doc "Demand at market j in cases")


      (parameter d :domain {I J} :doc "Distance in thousands of miles")

      (parameter f :doc "Freight in dollars per case per thousand miles")

      (parameter c :domain {I J}
                 :doc "Transport cost in thousands of dollars per case")

      (variable-declaration x
                            :domain {I J}
                            :doc "Shipment quantities in cases")



      (minimize "cost" (sumf ((k in I))
                            (sumf ((l in J))
                                  (* [c k l]
                                     [x k l])))
                :doc "Total transportation costs in thousands of dollars.")

      (constraint "supply"
                  (<= (sumf ((l in J))
                            [x k l])
                      [a k])
                  :quantifiers ((for-all-quantifier k I))
                  :doc "Observe supply limit at plant i")

      (constraint "demand"
                  (>= (sumf ((k in I))
                            [x k l])
                      [b l])
                  :quantifiers ((for-all-quantifier l J))
                  :doc "Satisfy demand at market J")


      (data-section
       (set-value-of-set I (list "Seattle" "San-Diego"))

       (set-value-of-set J (list "New-York" "Chicago" "Topeka"))

       (set-value-of-param a '(350 600))

       (set-value-of-param b '(325 300 275))

       (set-value-of-param d '((2.5 1.7 1.8)
                               (2.5 1.8 1.4)))

       (set-value-of-param f 90)

       ;; c[i,j] = f * d[i, j] / 1000
       ;; c[i,j] = d[i, j] * 0.09
       (set-value-of-param c (list (list (cl:* 0.09 2.5)
                                    (cl:* 0.09 1.7)
                                    (cl:* 0.09 1.8))
                                   (list (cl:* 0.09 2.5)
                                    (cl:* 0.09 1.8)
                                    (cl:* 0.09 1.4)))))))
