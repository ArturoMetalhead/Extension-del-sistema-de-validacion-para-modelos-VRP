(in-package :lmml)

(defmethod value ((obj number))
  "If it is a number, just return it."
  obj)

(defmethod value ((obj param-ref))
  "If it is a param-ref, just return the value in the original declaration."
  (value (original-declaration obj)))

(defmethod value ((obj set-ref))
  "If it is a set-ref, just return the value in the original declaration."
  (value (original-declaration obj)))

(generate-value-for-binary-operators
  ((add  cl:+)
   (subs cl:-)
   (mult cl:*)
   (div  cl:/)))

(defmethod current-value ((obj number))
  "If it is a number, just return it."
  obj)

(defmethod current-value ((obj param-ref))
  "If it is a param-ref, just return the current-value in the original declaration."
  (current-value (original-declaration obj)))

(defmethod current-value ((obj set-ref))
  "If it is a set-ref, just return the current-value in the original declaration."
  (current-value (original-declaration obj)))

(generate-current-value-for-binary-operators
  ((add  cl:+)
   (subs cl:-)
   (mult cl:*)
   (div  cl:/)))

(defmethod domain ((obj param-ref))
  "If it is a param-ref, return the domain in the original declaration."
  (domain (original-declaration obj)))

(defmethod domain ((obj var-ref))
  "If it is a set-ref, return the domain in the original declaration."
  (domain (original-declaration obj)))

(defmethod name ((obj number))
  "If it is a number, return a string with it."
  (mkstr obj))

(defmethod name ((obj symbol))
  "If it is a symbol, return a string with it."
  (mkstr obj))

(defmethod name ((obj string))
  "If it is a string, just return it."
  obj)

(defmethod name ((obj set-value-of-set))
  "If it is a set-value-of-set, return the name of the set-name."
  (name (set-name obj)))

(defmethod name ((obj set-value-of-param))
  "If it is a set-value-of-param, return the name of the param-name."
  (name (param-name obj)))

(defmethod generate-value-of-set ((node list)
                                  (data-reader standard-data-reader))
  "If it is a list, just return it."
  node)

(defmethod generate-value-of-set ((node range-node)
                                  (data-reader standard-data-reader))
  "If it is a range-node, generate a list with the elements in that range."
  (let* ((min (value (min-value node)))
         (max (value (max-value node)))
         (inc (value (increment node))))
    (if (cl:> inc 0)
        (loop for i from min to max by inc
              collecting i)
        (loop for i from min downto max by (cl:- inc)
               collecting i))))

(defmethod make-dummy-variable-name-generator ((lang t))
  "If it is a list, just return it."
  (let* ((names-list '("i" "j" "k" "l"))
         (current-number 1)
         (current-index 0))
    (lambda ()
      (prog1
          (concatenate
           'string
           (string-downcase (nth current-index names-list))
           (if (cl:> current-number 1)
               (mkstr current-number)
               ""))
        (incf current-index)
        (when (cl:>= current-index (length names-list))
         (setf current-index 0)
         (incf current-number))))))
