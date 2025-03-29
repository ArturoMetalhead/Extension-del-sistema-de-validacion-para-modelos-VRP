(in-package :vrp)

    (defgeneric get-prerequisites (characteristic high-level-strategy))
    ;; Generic function that for a characteristic and high-level-strategy pair 
;; get the list of prerequisites of that characteristic on that high level strategy


;; This function return the code for a specific characteristic and high-level-strategy
;; of their respective get-prerequisites method, is used in a macro that creates all those functions.   
    (defun prerequisite-characteristics-code (characteristic prerequisite-classes high-level-strategy)
	  `(defmethod get-prerequisites ((characteristic (eql ',characteristic)) (high-level-strategy (eql ',high-level-strategy)))
	     ',prerequisite-classes))

(defun topologic-sort (characteristics high-level-strategy  &aux (visited ()) (topo ()))
	(labels ((dfs* (c)
		(push c visited)
		(let    ((prerequisites (get-prerequisites c high-level-strategy))) 
                (loop for prerequisite-characteristic in prerequisites
				    when (and (not (member prerequisite-characteristic visited)) (member prerequisite-characteristic characteristics))
				    do  (dfs* prerequisite-characteristic)))
		    (push c topo)))

	    (loop for c in characteristics
		    when (not (member c visited))
		    do (dfs* c)))
	    topo)
