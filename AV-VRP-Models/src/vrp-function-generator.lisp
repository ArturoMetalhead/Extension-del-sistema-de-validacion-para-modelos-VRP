(in-package :lmml)

(defclass lisp-lang (basic-language) ())

(defun lisp-lang () 
  (make-instance 'lisp-lang 
		  :union-symbol "union"
		  :intersection-symbol "inter"
		  :assignment-symbol "="
		  :difference-symbol "diff"
		  :cartesian-product-symbol "cross"))

(defparameter lisp (lisp-lang))

(generate-code-for-binary-operators
     lisp-lang
     "(~a ~a ~a)"
     (op left right)
     *basic-language-binary-operators*)


    (defmethod generate-code ((node symbol)
			   (lang lisp-lang)
			   stream)
   (format stream "~a" (symbol-name node)))

(gcode is-a-reference lisp-lang ("~a") (name))

(defmethod generate-code((node index-at)
                         (lang lisp-lang)
                         (stream t))

  (let* ((indexes-code (mapcar (lambda (x) (generate-code x lang nil))
                               (indexes node))))
    (format stream "(aref ~a ~{~a ~})"
            (gcodenil var-name)
            indexes-code)))

(gcode var-in-set lisp-lang ("~a in ~a") (var-name set-name))

(defmethod generate-code ((node sum-node)
			    (lang lisp-lang)
				      stream)
    (let* ((length (loop for i in (sum-bounds node) sum (if (or (subtypep (type-of i) 'var-in-set)
								(subtypep (type-of i) 'var-from-to))
									    1 0)))
	   (end-line (make-string length :initial-element #\)))
	   (loop-when (mapcar (lambda (x) (if (or (subtypep (type-of x) 'var-in-set)
						  (subtypep (type-of x) 'var-from-to))
						    "(loop for" "when" ))
					    (sum-bounds node))))


	    (format stream "~:{ ~a ~a ~%~t~} sum ~a~a~% "
		     (mapcar (lambda (y x) (list y (gcodenil-exp x)))
				    loop-when (sum-bounds node))
				    (gcodenil elements) end-line)))

(gcode for-all-quantifier lisp-lang ("loop for ~a in ~a~%~t when (not ~a") (var-name set-name (if (pred node) (format nil "(or (not ~a) " (gcodenil pred)) "" ) ))

(defparameter *my-var-param-set-list* '(x y K C M P))

(defmethod generate-code ((node constraint-node)
			  (lang lisp-lang)
			  stream)
  (let ((main-code-string (generate-code (func node) lang nil)))
  (if (quantifiers node)
      (format stream "(defun ~a ~a~%~t~a)"
	      (generate-code (id node) lang nil)
	      *my-var-param-set-list*
	      (build-for-all-string (quantifiers node) main-code-string lang))

      (format stream "(defun ~a ~a~%~t~a)"
	      (generate-code (id node) lang nil)
	      *my-var-param-set-list*
	       main-code-string))))

(defun build-for-all-string (for-all-list main-code-string lang)
   (if (eq (length for-all-list) 1) 
      (format nil "(~a ~a~a)~% ~a)" (generate-code (car for-all-list) lang nil)
				main-code-string
			       (if (pred (car for-all-list)) ")"  "" )
			       (build-return-finally (generate-code (var-name (car for-all-list)) lang nil) ))
      (format nil "(~a~a~a)~% ~a)"
	      (generate-code (car for-all-list) lang nil)
	      (build-for-all-string (cdr for-all-list) main-code-string lang)
	      (if (pred (car for-all-list)) ")"  "" )
	      (build-return-finally (generate-code (var-name (car for-all-list)) lang nil)))))

(defun build-return-finally (iter-name)
  (format nil "do (format stream \"~t error detected in ~a: ~a\" ~a)~%(return nil)~%finally (return t)" iter-name "~a~%" iter-name))

(gcode range-node lisp-lang
       ("from ~a to ~a~a")
       (min-value
        max-value
        (if (and (numberp (increment node))
                 (/= (increment node) 1))
            (format nil " by ~a"
                    (gcodenil increment))
            "")))

(gcode objective-function-node lisp-lang ("") nil)

(gcode parameter-declaration-node lisp-lang ("(defparameter ~a nil)") (name))

(gcode set-declaration-node lisp-lang ("(defparameter ~a nil)") (name))

(defmethod generate-code ((node variable-declaration-node)
			  (lang lisp-lang)
				   stream)
  (format stream "(defparameter ~a nil)~%" (name node))
  (if (lower-bound node)
	 (format stream "~a~%" (build-st-for-bound
		   (format nil "~a-lower-bound" (name node)) "<=" node 'lower-bound lang)))

  (if (upper-bound node)
	  (format stream "~a~%"  (build-st-for-bound (format nil "~a-upper-bound" (name node)) ">=" node 'upper-bound lang ))))

(defun build-st-for-bound (function-name operation node bound lang)
      (if (domain node)

	 (let* ((domain-indexes  (loop for i from 0 to (1- (length (domain node)))
					 collect (symb "i" i)))
		(aref-text (format nil "(aref ~a ~{~a ~})" (name node) domain-indexes))
		(condition-text (format nil "(~a ~a ~a)" operation (eval`(,bound ,node))  aref-text))
		(quantifiers-list (loop for i from 0 to (1- (length (domain node)))
					 collect (make-instance 'for-all-quantifier
						 :var-name (symb "i" i)
						 :pred nil
						 :set-name (nth i (domain node))))))
	  (format nil "(defun ~a ~a~%~t~a)" function-name
		   *my-var-param-set-list*
		   (build-for-all-string quantifiers-list condition-text lang)) )

	(format nil "(defun ~a ~a~%~t(~a ~a ~a))" function-name
						 *my-var-param-set-list*
						 operation (eval`(,bound ,node)) (name node))))




(defmethod generate-code ((node problem-definition-node)
			    (lang lisp-lang)
			   stream)
  (let* ((func-list '()))

      (loop for inst in (elements (instr node))
	do (cond ((subtypep (type-of inst) 'variable-declaration-node)
	     (progn
	       (if (lower-bound inst)
		 (push (symb (format nil "~a-lower-bound" (name inst))) func-list))
	       (if (upper-bound inst)
		 (push (symb (format nil "~a-upper-bound" (name inst))) func-list))))

	      ((subtypep (type-of inst) 'constraint-node)
		  (push (symb (id inst)) func-list)))
	 )

	(format stream "(in-package :vrp)~%")
	(format stream "~a" (gcodenil-exp (instr node)))
	func-list
      ))



; (defmethod generate-code ((node problem-definition-node)
;                            (lang lisp-lang)
;                            stream)
;   (let* ((restricciones (elements (instr node)))  ;; Obtiene las restricciones existentes
;          (nueva-restriccion (constraint "r8" (<= (sumf ((b in I)) [x 0 b]) K)))  ;; Define la nueva restricción
;          (func-list '()))  ;; Inicializa la lista de funciones

;     ;; Agrega la nueva restricción al final de la lista de restricciones
;     (push nueva-restriccion restricciones)

;     ;; Itera sobre las restricciones (incluyendo la nueva)
;     (loop for inst in restricciones
;           do (cond ((subtypep (type-of inst) 'variable-declaration-node)
;                      (progn
;                        (when (lower-bound inst)
;                          (push (symb (format nil "~a-lower-bound" (name inst))) func-list))
;                        (when (upper-bound inst)
;                          (push (symb (format nil "~a-upper-bound" (name inst))) func-list))))
;                     ((subtypep (type-of inst) 'constraint-node)
;                      (push (symb (id inst)) func-list))))

;     (format stream "(in-package :vrp)~%")
;     (format stream "~a" (gcodenil-exp (instr node)))
;     func-list))



;;;;;;;;;;CREO QUE TENDRIA QUE PASARLE EL SYMB MEANING DESDE EL MODEL EVALUATOR A ESTO


; (defparameter *variable-restriction-map* (make-hash-table))

; (defun add-restriction-function (var-name restriction-function)
;   "Asocia una variable con su función de restricción."
;   (setf (gethash var-name *variable-restriction-map*) restriction-function))

; (defun generate-restriction-for-variable (var-name)
;   "Genera la restricción para la variable dada si existe en el mapa."
;   (let ((restriction-function (gethash var-name *variable-restriction-map*)))
;     (when restriction-function
;       (funcall restriction-function))))

; (defun restriction-for-x ()
;   "Ejemplo de restricción para la variable 'x'."
;   ;; Aquí defines la lógica de la restricción para 'x'
;   "(defun restriction-x () ... )")

; (defun restriction-for-y ()

;   "Ejemplo de restricción para la variable 'y'."

;   (let ((y 'y)   ;; Nombre de la variable y

;         (u 'u)   ;; Nombre de la variable u

;         (P 'P)   ;; Nombre de la variable P

;         (x 'x))  ;; Nombre de la variable x

;     ;; Genera la restricción y + u = P * x

;     (format nil "(defun restriction-y () 

;                    (assert (= (+ ~a ~a) (* ~a ~a))) 

;                    (return t))" y u P x)))

; ;; Agregar funciones de restricción al mapa
; (add-restriction-function 'x #'restriction-for-x)
; (add-restriction-function 'y #'restriction-for-y)

; (defmethod generate-code ((node problem-definition-node)
;                           (lang lisp-lang)
;                           stream)
;   (let* ((func-list '()))
    
;     ;; Recorre los elementos del problema
;     (loop for inst in (elements (instr node))
;           do (cond ((subtypep (type-of inst) 'variable-declaration-node)
;                      (progn
;                        (if (lower-bound inst)
;                            (push (symb (format nil "~a-lower-bound" (name inst))) func-list))
;                        (if (upper-bound inst)
;                            (push (symb (format nil "~a-upper-bound" (name inst))) func-list))
                       
;                        ;; Genera restricciones para la variable
;                        (generate-restriction-for-variable (name inst))))

;                     ((subtypep (type-of inst) 'constraint-node)
;                      (push (symb (id inst)) func-list))))

;     (format stream "(in-package :vrp)~%")
;     (format stream "~a" (gcodenil-exp (instr node)))
;     func-list
;     ))























;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;blackbox


; (defmethod generate-code ((node problem-definition-node)
;                           (lang lisp-lang)
;                           stream)
;   (let* ((func-list '())
;          (new-constraint nil))  ;; Variable para la nueva restricción

;     ;; Recorre los elementos del problema
;     (loop for inst in (elements (instr node))
;           do (cond ((subtypep (type-of inst) 'variable-declaration-node)
;                      (progn
;                        (if (lower-bound inst)
;                            (push (symb (format nil "~a-lower-bound" (name inst))) func-list))
;                        (if (upper-bound inst)
;                            (push (symb (format nil "~a-upper-bound" (name inst))) func-list))
;                        ;; Verifica si la variable 'x' existe
;                        (when (string= (symbol-name (name inst)) "x")
;                          ;; Define la nueva restricción
;                          (setf new-constraint (format nil "(defun new-constraint () ... )")))))

;                     ((subtypep (type-of inst) 'constraint-node)
;                      (push (symb (id inst)) func-list))))

;     ;; Si la nueva restricción fue definida, agrégala al stream
;     (when new-constraint
;       (format stream "~a~%" new-constraint))

;     (format stream "(in-package :vrp)~%")
;     (format stream "~a" (gcodenil-exp (instr node)))
;     func-list
;     ))


;;;;;;;;;;;;;;;;;;;;;;;segunda de blackbox



; (defparameter *variable-restriction-map* (make-hash-table))

; (defun add-restriction-function (var-name restriction-function)
;   "Asocia una variable con su función de restricción."
;   (setf (gethash var-name *variable-restriction-map*) restriction-function))

; (defun generate-restriction-for-variable (var-name)
;   "Genera la restricción para la variable dada si existe en el mapa."
;   (let ((restriction-function (gethash var-name *variable-restriction-map*)))
;     (when restriction-function
;       (funcall restriction-function))))

; (defun restriction-for-x ()
;   "Ejemplo de restricción para la variable 'x'."
;   ;; Aquí defines la lógica de la restricción para 'x'
;   "(defun restriction-x () ... )")

; (defun restriction-for-y ()
;   "Ejemplo de restricción para la variable 'y'."
;   ;; Aquí defines la lógica de la restricción para 'y'
;   "(defun restriction-y () ... )")

; ;; Agregar funciones de restricción al mapa
; (add-restriction-function 'x #'restriction-for-x)
; (add-restriction-function 'y #'restriction-for-y)

; (defmethod generate-code ((node problem-definition-node)
;                           (lang lisp-lang)
;                           stream)
;   (let* ((func-list '()))
    
;     ;; Recorre los elementos del problema
;     (loop for inst in (elements (instr node))
;           do (cond ((subtypep (type-of inst) 'variable-declaration-node)
;                      (progn
;                        (if (lower-bound inst)
;                            (push (symb (format nil "~a-lower-bound" (name inst))) func-list))
;                        (if (upper-bound inst)
;                            (push (symb (format nil "~a-upper-bound" (name inst))) func-list))
                       
;                        ;; Genera restricciones para la variable
;                        (generate-restriction-for-variable (name inst))))

;                     ((subtypep (type-of inst) 'constraint-node)
;                      (push (symb (id inst)) func-list))))

;     (format stream "(in-package :vrp)~%")
;     (format stream "~a" (gcodenil-exp (instr node)))
;     func-list
;     ))




;;;;;;;;;;;;;;;;;;el de chatgpt


; (defmethod generate-code ((node problem-definition-node) (lang lisp-lang) stream)
;   (let* ((func-list '())
;          (var-exists nil)) ; Variable para verificar existencia

;     (loop for inst in (elements (instr node))
;           do (cond 
;               ;; Verificar declaraciones de variables
;               ((subtypep (type-of inst) 'variable-declaration-node)
;                (progn
;                  ;; Verificar límites
;                  (if (lower-bound inst)
;                      (push (symb (format nil "~a-lower-bound" (name inst))) func-list))
;                  (if (upper-bound inst)
;                      (push (symb (format nil "~a-upper-bound" (name inst))) func-list))
;                  ;; Verificar si la variable específica existe
;                  (when (eq 'x (name inst)) ; Cambia 'x por el nombre de tu variable
;                    (setf var-exists t))))

;               ;; Verificar nodos de restricciones
;               ((subtypep (type-of inst) 'constraint-node)
;                (push (symb (id inst)) func-list))))

;     ;; Agregar nueva restricción si la variable existe
;     (when var-exists
;       ;; Aquí defines tu nueva restricción, por ejemplo:
;       ;; Supongamos que queremos agregar "x <= 10"
;       (push '(constraint "new-restriction" (< x 10)) func-list))

;     ;; Generar el código para el paquete y las funciones
;     (format stream "(in-package :vrp)~%")
;     (format stream "~a" (gcodenil-exp (instr node)))
;     func-list))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defun checking-variable-meaning-exist ())

; (defun add-uij-constraint (problem-solution)
;   "Agrega la restricción para u_ij si está presente en el problema."
;   (let ((uij (find-variable 'uij problem-solution))
;         (xij (find-variable 'xij problem-solution)))
;     (when uij ; Verifica si u_ij está definido
;       (loop for i from 0 below *num-customers*
;             do (loop for j from 0 below *num-customers*
;                       when (< i j)
;                       do (let ((constraint (format nil "~A + ~A = ~A * ~A" 
;                                                     (aref uij i j)
;                                                     (aref uij j i)
;                                                     *capacity*
;                                                     (aref xij i j))))
;                            ;; Aquí agregas la restricción al problema
;                            (add-constraint-to-problem problem-solution constraint)))))))

; (defun find-variable (var-name problem-solution)
;   "Busca una variable por nombre en el problema."
;   ;; Aquí deberías implementar la lógica para encontrar la variable
;   ;; Esto depende de cómo esté estructurada tu solución del problema.
;   ;; Por ejemplo:
;   (case var-name
;     ((uij) (getf problem-solution :uij))
;     ((xij) (getf problem-solution :xij))
;     ;; Agrega más variables según sea necesario
;     ))


; (defun add-constraint-to-problem (problem-solution constraint)
;   "Agrega una restricción al problema."
;   ;; Implementa aquí cómo se agregan las restricciones a tu estructura de problema.
;   ;; Por ejemplo:
;   (push constraint (getf problem-solution :constraints)))


; (defun solve-vrp (problem-solution)
;   "Resuelve el VRP y agrega restricciones necesarias."
;   ;; Define tus variables aquí...
  
;   ;; Agrega restricciones significativas
;   (add-uij-constraint problem-solution)

;   ;; Procede con el resto del proceso de solución...
; )