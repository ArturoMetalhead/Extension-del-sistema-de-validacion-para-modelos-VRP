(in-package :vrp)

(defun make-slot-by-name (slot-name) 
    `(,slot-name
	    :accessor ,slot-name
	    :initarg ,(make-keyword slot-name)
	    :initform nil
	    ))

;; Macro that given a required field X creates a class HAS-X with an slot X
(defmacro def-requirement-class (slot-name)
	`(defclass ,(intern (format nil "HAS-~a" slot-name)) ()
	    (,(make-slot-by-name slot-name))))

(def-requirement-class clients)

(def-requirement-class routes) 

(def-requirement-class demand)

(def-requirement-class capacity)

(def-requirement-class distance)

(defgeneric get-required-classes (characteristic high-level-strategy))

(defun required-classes-code (characteristic required-classes high-level-strategy)
    `(defmethod get-required-classes ((characteristic (eql ',characteristic)) (high-level-strategy (eql ',high-level-strategy)))
        ',required-classes))

(defmacro def-problem-solution-pair (name high-level-strategy vrp-description)

    (let*   ((description (get-description-elements vrp-description))
	     (required-classes '()) )
	    (loop for d in description
		  do (setf required-classes (append required-classes (get-required-classes d high-level-strategy)) ))

	    (setf required-classes (remove-duplicates required-classes))
     `(defclass ,name ,required-classes ())))
