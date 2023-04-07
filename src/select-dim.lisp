
(in-package :veq)

; TODO: symbol passthrough
(defmacro vsel ((type &rest dims) &body body)
  "return values from body in order of dims.
use indices or :x :y :z :w
ex: (VSEL (df :w :zx 0) (values a b c d)) returns: (values d c a a)."
  (let ((a '((:x 0) (:y 1) (:z 2) (:w 3)))
        (mx 0) (dimgs (list)) (ign (list)))
    (labels
      ((ign (&aux (s (gensym "IGN"))) (push s ign) s)
       (gs-dim (i &aux (gs (gensym "VAR")))
         (setf mx (max mx i) dimgs (acons i gs dimgs))
         `(,i . ,gs))
       (proc-sel (d)
         (let* ((i (etypecase d
                     (fixnum d)
                     (keyword (the pn (cadr (assoc d a))))))
                (igs (assoc i dimgs)))
           (if igs igs (gs-dim i))))
       (explode (dims)
         (loop for d in dims
               nconc (etypecase d
                       (fixnum (list d))
                       (keyword (map 'list (lambda (s) (psymb :keyword s))
                                     (symbol-name d)))))))
      (let* ((dims (explode dims))
             (gs (loop for d in dims collect (proc-sel d)))
             (mvb (loop for i from 0 to mx
                        if (cdr (assoc i gs)) collect (cdr (assoc i gs))
                        else collect (ign)))
             (vals (loop for s in gs collect (cdr s))))
        `(mvb (,@mvb) (~ ,@body)
           (declare (ignore ,@ign) (,type ,@(remove-duplicates vals)))
           (values ,@vals))))))

(defmacro define-vsel (ty &aux (mname (veqsymb 1 ty :sel)))
  `(progn
     (export ',mname)
     (defmacro ,mname ((&rest dims) &body body)
     ,(format nil "return values from body in order of dims.
use indices or :x :y :z :w
ex: (~s (:w :zx 0) (values a b c d)) returns: (values d c a a)." mname)
    `(vsel (,',ty ,@dims) ,@body))))
(define-vsel df) (define-vsel ff) (define-vsel in) (define-vsel pn)

