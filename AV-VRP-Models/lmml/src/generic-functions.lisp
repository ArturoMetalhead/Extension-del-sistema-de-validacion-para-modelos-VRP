(in-package :lmml)

(defgeneric length (obj)
  (:documentation "Returns the length of the given obj."))

(defgeneric write-name (obj lang stream)
  (:documentation "Defines how to write the name of the given object in the given language to the specified stream."))

(defgeneric format-value-of (obj value lang data-reader stream)
  (:documentation "Defines how to format the value of the given object in the given language using the specified data-reader."))

(defgeneric generate-value-of-set (obj data-reader)
  (:documentation "Returns a lisp object with the elements specified in the obj."))

(defgeneric make-dummy-variable-name-generator (language)
   (:documentation "Returns a closure that every time it is called returns a new name for a dummy variable."))

(defgeneric convert-to-underscore (obj)
  (:documentation "Returns a string with the obj in underscore_notation."))
