(in-package :vrp)

(defun random-choice (l &optional choice-index)
  (let* ((choice (if (null choice-index)
		     (random (length l))
		   choice-index)))
    (nth choice l)))

(defun permutation (l)
  (if (null l)
      '()
    (let* ((element (random-choice l)))
      (cons element (permutation (remove element l :count 1))))))

(defun nthcar (n l)
    (loop for i from 0 to (1- n)
	  collect (nth i l)))

(defun k-partition (k l)
  (if (<= k 1)
      `(,l)
    (let* ((i (1+ (random (- (length l) (1- k))))))
      (cons (nthcar i l) (k-partition (1- k) (nthcdr i l))))))

