(in-package :vrp)
(defparameter V nil)

(defparameter I nil)

(defparameter C nil)

(defparameter D nil)

(defparameter N nil)

(defparameter P nil)

(defparameter K nil)

(defparameter X nil)
(defun X-lower-bound (V I D N P K X Y STREAM)
 (loop for I0 in V
  when (not (loop for I1 in V
  when (not  (<= 0 (aref X I0 I1 )))
 do (format stream "  error detected in I1: ~a~%" I1)
(return nil)
finally (return t)))
 do (format stream "  error detected in I0: ~a~%" I0)
(return nil)
finally (return t)))


(defparameter Y nil)
(defun Y-lower-bound (V I D N P K X Y STREAM)
 (loop for I0 in V
  when (not (loop for I1 in V
  when (not  (<= 0 (aref Y I0 I1 )))
 do (format stream "  error detected in I1: ~a~%" I1)
(return nil)
finally (return t)))
 do (format stream "  error detected in I0: ~a~%" I0)
(return nil)
finally (return t)))




(defun r1 (V I D N P K X Y STREAM)
 (loop for A in I
  when (not  (=  (loop for B in V 
  sum (- (aref Y B A ) (aref Y A B )))
  (* 2 (aref D A ))))
 do (format stream "  error detected in A: ~a~%" A)
(return nil)
finally (return t)))

(defun r2 (V I D N P K X Y STREAM)
 (=  (loop for B in I 
  sum (aref Y 0 B ))
   (loop for A in I 
  sum (aref D A ))
 ))

(defun r3 (V I D N P K X Y STREAM)
 (=  (loop for B in I 
  sum (aref Y B 0 ))
  (- (* K P)  (loop for A in I 
  sum (aref D A ))
 )))

(defun r4 (V I D N P K X Y STREAM)
 (=  (loop for B in I 
  sum (aref Y (+ N 1) B ))
  (* K P)))

(defun r6 (V I D N P K X Y STREAM)
 (loop for A in I
  when (not  (=  (loop for B in V 
  sum (aref X A B ))
  1))
 do (format stream "  error detected in A: ~a~%" A)
(return nil)
finally (return t)))

(defun r7 (V I D N P K X Y STREAM)
 (loop for A in I
  when (not  (=  (loop for B in V 
  sum (aref X B A ))
  1))
 do (format stream "  error detected in A: ~a~%" A)
(return nil)
finally (return t)))