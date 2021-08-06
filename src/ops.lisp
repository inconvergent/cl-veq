
(in-package :veq)

;;;;;;;;;;;;;;;;;;

; (declaim (inline d$copy))
(defun d$copy (a)
  (declare #.*opt* (dvec a))
  (make-array (length a) :initial-contents a :element-type 'df :adjustable nil))

; (declaim (inline f$copy))
(defun f$copy (a)
  (declare #.*opt* (fvec a))
  (make-array (length a) :initial-contents a :element-type 'ff :adjustable nil))
