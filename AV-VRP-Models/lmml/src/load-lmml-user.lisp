(defun fload (filename)
     (let* ((dot-string (make-string (cl:- 40 (cl:+ 3 (length filename)))
                                     :initial-element #\.)))
       (format t "Loading ~a ..." filename)
       (load filename)
       (format t "~a OK~%" dot-string)))

;; (fload "src/lmml-user-package.lisp")
(fload "src/lmml-case-sensitivity.lisp")
