(in-package :lmml)

(defun open-brace-and-evaluate-macro-character (stream char1)
  (declare (ignore char1))
  `(list ,@(read-delimited-list #\} stream t)))

(set-macro-character #\{ #'open-brace-and-evaluate-macro-character)
(set-macro-character #\} (get-macro-character #\)))

(defun open-bracket-macro-character (stream char)
  (declare (ignore char))
  `(index-at ,@(read-delimited-list #\] stream t)))


(set-macro-character #\[ #'open-bracket-macro-character)
(set-macro-character #\] (get-macro-character #\)))
