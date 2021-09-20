
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
        `(progn (export ',in)
                (export ',out)
                (export ',inout)
                (vdef ,in ,args (let ((,x* (,(veqsymb 1 type "CLAMP") ,x*))) ,@body))
                (vdef ,out ,args (let ((,x* (,(veqsymb 1 type "CLAMP") (- 1 ,x*))))
                                   (1+ (- ,@body))))
                (vdef ,inout ,args
                  (let ((,x* (,(veqsymb 1 type "CLAMP") ,x*)))
                    (if (< ,x* 0.5) (let ((,x* (* 2 ,x*))) (* 0.5 ,@body))
                                    (let ((,x* (- 1 (* 2 (- ,x* 0.5)))))
                                      (+ 0.5 (* (+ 1 (- ,@body)) 0.5)))))))))))

(easing-op ff linear (x) x)
(easing-op ff sin (x) (- 1 (cos (* x fpi5))))
(easing-op ff cubic (x) (* x x x))
(easing-op ff quart (x) (expt x 4))
(easing-op ff quint (x) (expt x 5))
(easing-op ff exp (x) (expt 2 (* 10 (- x 1))))
(easing-op ff circ (x) (- (- (sqrt (- 1 (* x x))) 1)))
(easing-op ff elastic (x &optional (p 0.3) (s nil))
  (let ((s (or s (* (asin 1) (/ p fpii)))))
    (- (* (expt 2 (* 10 (- x 1))) (sin (/ (* (- (- x 1) s) fpii) p))))))
(easing-op ff back (x &optional (s 1.70158))
  (* x x (- (* (+ 1 s) x) s)))

(easing-op df linear (x) x)
(easing-op df sin (x) (- 1 (cos (* x fpi5))))
(easing-op df cubic (x) (* x x x))
(easing-op df quart (x) (expt x 4))
(easing-op df quint (x) (expt x 5))
(easing-op df exp (x) (expt 2 (* 10 (- x 1))))
(easing-op df circ (x) (- (- (sqrt (- 1 (* x x))) 1)))
(easing-op df elastic (x &optional (p 0.3) (s nil))
  (let ((s (or s (* (asin 1) (/ p fpii)))))
    (- (* (expt 2 (* 10 (- x 1))) (sin (/ (* (- (- x 1) s) fpii) p))))))
(easing-op df back (x &optional (s 1.70158))
  (* x x (- (* (+ 1 s) x) s)))

