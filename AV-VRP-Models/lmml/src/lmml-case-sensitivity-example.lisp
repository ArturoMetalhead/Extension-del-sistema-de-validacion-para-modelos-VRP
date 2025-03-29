(defparameter lc-example-1
  (problem-node "lower-case example1"
                (variable-declaration x :var-type binary-variable)
                (variable-declaration y)

                (maximize "obj" (+ (* 4 x)
                                   (* 2 y)))

                (constraint "r1" (<= (+ x y) 3))))

(defparameter lc-example-2
  (problem-node "lowercase example 2"
                (parameter c1 :value 4)
                (parameter c2 :value 2)
                (variable-declaration x :var-type binary-variable)
                (variable-declaration y)

                (maximize "obj" (+ (* c1 x)
                                   (* c2 y)))

                (constraint "r1" (<= (+ x y) 3))))

(defparameter lc-example-3
  (problem-node "lowercase example 3"
                (set I :value {1 2})
                (parameter c1 :value 4)
                (parameter c2 :value 2)
                (variable-declaration x :domain {I}
                                      :var-type binary-variable)

                (maximize "obj" (+ (* c1 (index-at x 1))
                                   (* c2 (index-at x 2))))

                (constraint "r1" (<= (+ (index-at x 1)
                                        (index-at x 2))
                                     3))))

(defparameter lc-example-4
  (problem-node "example5"
                (parameter n :value 5)
                (set I :value (list (range 1 (+ n 1))))
                (parameter c1 :value 4)
                (parameter c2 :value 2)
                (variable-declaration x :domain (list I)
                                      :var-type binary-variable)

                (maximize "obj" (+ (* c1 (index-at x 1))
                                   (* c2 (index-at x 2))))

                (constraint "r1" (<= (+ (index-at x 1)
                                             (index-at x 2))
                                          n))

                (solve)
                (display x)
                ))

(defparameter lc-example-5
  (problem-node "lowercase example 5"
                (set I :value {1 2})
                (parameter c1 :value 4)
                (parameter c2 :value 2)
                (variable-declaration x :domain {I I}
                                      :var-type binary-variable)

                (maximize "obj" (+ (* c1 [x 1 1])
                                   (* c2 [x 2 1])))

                (constraint "r1" (<= (+ [x 1 1]
                                        [x 2 1])
                                     3))))

(defparameter lc-cvrp1-example
  (problem-node "cvrp1"
      (parameter n :doc "Number of clients")
      (parameter P :doc "Capacity of the vehicles")
      (parameter K :doc "Number of vehicles")
      (parameter M :doc "Sum of the demands")
      (set V :value {(range 1 n)} :doc "Set of clients")
      (set VD :value {(range 0 (+ n 1))} :doc "Clients and depots")

      (parameter c :domain {V V} :doc "Distance betweens clients")
      (parameter d :domain {V} :doc "Demand of each client")


      (variable-declaration x
                            :var-type binary-variable
                            :domain {VD VD}
                            :doc "1 if client i is visited right before client j.")

      (variable-declaration y
                            :domain {VD VD}
                            :doc "A variable to track the commodity flow")

      (minimize "obj" (sumf ((i in VD))
                            (sumf ((j in VD))
                                  (* [x i j]
                                     [c i j]))))

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

(defparameter lc-gmpl-example
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



          (minimize "cost" (sumf ((i in I))
                                (sumf ((j in J))
                                      (* [c i j]
                                         [x i j])))
                    :doc "Total transportation costs in thousands of dollars.")

          (constraint "supply"
                      (<= (sumf ((j in J))
                                [x i j])
                          [a i])
                      :quantifiers ((for-all-quantifier i I))
                      :doc "Observe supply limit at plant i")

          (constraint "demand"
                      (>= (sumf ((i in I))
                                [x i j])
                          [b j])
                      :quantifiers ((for-all-quantifier j J))
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
           (set-value-of-param c (list (list (CL:* 0.09 2.5)
                                        (CL:* 0.09 1.7)
                                        (CL:* 0.09 1.8))
                                       (list (CL:* 0.09 2.5)
                                        (CL:* 0.09 1.8)
                                        (CL:* 0.09 1.4)))))))
