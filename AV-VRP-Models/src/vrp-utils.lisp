(in-package :vrp)

(defun make-clients (&optional max-clients-number)

    (let* ((n (if (null max-clients-number) 
		 (random 15 2)
		 (random max-clients-number 1))))
	(loop for i from 1 to n 
	    collect i)))

(defun make-routes(client-list &optional routes-number)

    (let*   ((k (if (or (null routes-number) (>= routes-number (length client-list)))
		    (random (1- (length client-list)) 1) 
		    routes-number))
	    (client-permutation (permutation client-list)))
	    (k-partition k client-permutation)))

(defun power-set (set &optional (index 0))
   (if (= index (length set)) '(())
       (let* ((item-i (nth index set)) 
	      (without-i (power-set set (1+ index)))
	      (with-i (loop for list in without-i
			    collect (push item-i list))))
              (append with-i without-i)
	 )))
