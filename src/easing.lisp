
(in-package :veq)

; implemented in a simiar fashion to:
; https://github.com/vydd/easing/blob/master/src/easing.lisp

(defmacro easing-op (type name args &body body)
  (labels ((num-types (o)
             (cond ((numberp o) (coerce o type))
                   ((atom o) o)
                   ((consp o) (cons (num-types (car o))
                                    (num-types (cdr o)))))))
    (let ((x* (car args))
          (in (veqsymb 1 type (symb 'ease-in- name))) ; fease-in-linear
          (out (veqsymb 1 type (symb 'ease-out- name)))
          (inout (veqsymb 1 type (symb 'ease-in-out- name))))
      (num-types
        `(fvprogn (export ',in) (export ',out) (export ',inout)
                  (defun ,in ,args
                    (let ((,x* (,(veqsymb 1 type "CLAMP") ,x*))) ,@body))
                  (defun ,out ,args
                    (let ((,x* (,(veqsymb 1 type "CLAMP") (- 1 ,x*))))
                      (1+ (- ,@body))))
                  (defun ,inout ,args
                    (let ((,x* (,(veqsymb 1 type "CLAMP") ,x*)))
                      (if (< ,x* 1/2) (let ((,x* (* 2 ,x*))) (* 1/2 ,@body))
                                      (let ((,x* (- 1 (* 2 (- ,x* 1/2)))))
                                        (+ 1/2 (* (+ 1 (- ,@body)) 1/2)))))))))))

(easing-op ff linear (x) x)
(easing-op ff sin (x) (- 1f0 (cos (* x fpi5))))
(easing-op ff cubic (x) (* x x x))
(easing-op ff quart (x) (expt x 4f0))
(easing-op ff quint (x) (expt x 5f0))
(easing-op ff exp (x) (expt 2f0 (* 10f0 (- x 1f0))))
(easing-op ff circ (x) (- (- (sqrt (- 1f0 (* x x))) 1f0)))
(easing-op ff elastic (x &optional (p 0.3) (s nil))
  (let ((s (or s (* (asin 1f0) (/ p fpii)))))
    (- (* (expt 2f0 (* 10f0 (- x 1f0))) (sin (/ (* (- (- x 1f0) s) fpii) p))))))
(easing-op ff back (x &optional (s 1.70158))
  (* x x (- (* (+ 1f0 s) x) s)))

(easing-op df linear (x) x)
(easing-op df sin (x) (- 1d0 (cos (* x dpi5))))
(easing-op df cubic (x) (* x x x))
(easing-op df quart (x) (expt x 4d0))
(easing-op df quint (x) (expt x 5d0))
(easing-op df exp (x) (expt 2d0 (* 10d0 (- x 1))))
(easing-op df circ (x) (- (- (sqrt (- 1d0 (* x x))) 1d0)))
(easing-op df elastic (x &optional (p 0.3) (s nil))
  (let ((s (or s (* (asin 1d0) (/ p dpii)))))
    (- (* (expt 2d0 (* 10d0 (- x 1))) (sin (/ (* (- (- x 1d0) s) dpii) p))))))
(easing-op df back (x &optional (s 1.70158))
  (* x x (- (* (+ 1d0 s) x) s)))

