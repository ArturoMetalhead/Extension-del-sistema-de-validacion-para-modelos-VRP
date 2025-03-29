(defparameter *infix-ops*
  '(((*) (/))
    ((+) (-))
    ((<) (>) (<=) (>=) (=))
    ((union) (inter) (diff) (cross)))
  "A list of lists of operators, highest precedence first.")

(defparameter *infix-ops-ast*
  (list (cons '+ 'add)
        (cons '- 'subs)
        (cons '* 'mult)
        (cons '/ 'div)
        (cons '+ 'add)
        (cons '< 'less)
        (cons '<= 'less-equal)
        (cons '> 'bigger)
        (cons '>= 'bigger-equal)
        (cons 'union 'union-op)
        (cons 'diff 'difference-op)
        (cons 'inter 'intersection-op)
        (cons 'cross 'cartesian-product-op)))

(defparameter *bin-ops-ast*
  `(add subs mult div less less-equal bigger bigger-equal equal))

(defparameter *first-elements-of-ast-nodes*
  `(index-at sumf))

(defun is-aritmetic-expression (expr)
  "Returns t if expr is a list that does not start with the constructor of an arithmetic operator."
  (if (or (member (first expr) *bin-ops-ast*)
          (member (first expr) *first-elements-of-ast-nodes*))
      ;; then
      nil
      ;;else
      t))
