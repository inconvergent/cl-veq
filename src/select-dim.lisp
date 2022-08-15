
(in-package :veq)

; TODO: symbol passthrough
(defmacro vsel ((type &rest dims) &body body)
  (let ((a '((:x 0) (:y 1) (:z 2) (:w 3)))
        (mx 0) (dimgs (list)) (ign (list)))
    (labels
       ((gs-dim (i &aux (gs (gensym "VAR")))
         (setf mx (max mx i) dimgs (acons i gs dimgs))
         `(,i . ,gs))
       (num (i)
         (unless (typep i 'integer) (error "bad dim in sel-dims: ~a" dims))
         i)
       (proc-sel (d)
         (let* ((i (typecase d
                     (fixnum d) (keyword (num (cadr (assoc d a))))))
                (igs (assoc i dimgs)))
           (if igs igs (gs-dim i))))
       (ign (&aux (s (gensym "IGN"))) (push s ign) s)
       ; this seems overly complicated
       (explode (dims)
          (awf (loop for d in dims
                     collect (typecase d
                               (fixnum d)
                               (keyword (map 'list
                                             (lambda (s) (psymb :keyword s))
                                             (symbol-name d))))))))

      (let* ((dims (explode dims))
             (gs (loop for d in dims collect (proc-sel d)))
             (mvb (loop for i from 0 to mx
                        if (cdr (assoc i gs)) collect (cdr (assoc i gs))
                        else collect (ign)))
             (vals (loop for s in gs collect (cdr s))))
        `(mvb (,@mvb) (~ ,@body)
           (declare (ignore ,@ign) (,type ,@(delete-duplicates vals)))
           (values ,@vals))))))

(defmacro fsel ((&rest dims) &body body)
  "return values from body in order of dims. use indices or :x :y :z :w
ex: (fsel (:w :zx 0) (values a b c d))
returns (values d c a a)."
  `(vsel (ff ,@dims) ,@body))
(defmacro dsel ((&rest dims) &body body)
  "return values from body in order of dims. use indices or :x :y :z :w
ex: (dsel (:w :zx 0) (values a b c d))
returns (values d c a a)."
  `(vsel (df ,@dims) ,@body))

