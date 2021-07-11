
(in-package :veq)

;;;;;;;;;;;;;;;;;;

(declaim (inline dcopy))
(defun dcopy (a)
  (declare #.*opt* (dvec a))
  (make-array (length a) :initial-contents a :element-type 'df :adjustable nil))

(declaim (inline fcopy))
(defun fcopy (a)
  (declare #.*opt* (fvec a))
  (make-array (length a) :initial-contents a :element-type 'ff :adjustable nil))
