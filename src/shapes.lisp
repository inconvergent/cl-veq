
(in-package :veq)

; TODO: type/dim template?

(fvdef* f2$rect (w h)
  (declare #.*opt* (ff w h))
  (f$_ `((,w ,(- h)) (,w ,h) (,(- w) ,h) (,(- w) ,(- h)))))
(fvdef* f2$square* (s) (declare #.*opt* (ff s)) (f2$rect s s))

(fvdef* f2$polygon (n rad &optional (rot 0f0))
  (declare #.*opt* (pos-int n) (ff rad rot))
  "return n-polygon of size rad. rotate by (rot 0)"
  (f$_ (loop with pin of-type ff = (/ fpii n)
             for i of-type pos-int from 0 below n
             collect (lst (f2scale (fcos-sin (+ rot (ff (* i pin)))) rad)))))

(fvdef* f2$circ (rad &optional (rs 0.5f0))
  (declare #.*opt* (ff rad rs))
  "return circle of size rad. (rs 0.5) is vertex density."
  (let ((n (ceiling (the ff (* fpii (the ff (* rad rs)))))))
    (declare (pos-int n))
    (f2$polygon n rad)))

(fvdef* f2$center (arr &aux (n (2$num arr)))
  (declare #.*opt* (fvec arr) (pos-int n))
  "center 2d array according to n points in array. n is optional."
  (veq:mvb ((varg 2 xx yy)) (f2$mima arr :n n)
    (f2!@$+ arr (fmid xx ) (fmid yy))))

(defmacro define-arr-shape (n sym)
  `(progn
    ,@(loop
        for (d ty*) in (group '(1 ff 2 ff 3 ff 4 ff 1 df 2 df 3 df 4 df
                                1 in 2 in 3 in 4 in 1 pn 2 pn 3 pn 4 pn) 2)
        for ty = (nth-value 1 (type-from-short ty*))
        for mname = (veqsymb d ty* sym)
        for e = (* n d)
        nconc `((export ',mname)
                (declaim (inline ,(symb :% mname)))
                (fvdef* ,mname ((:va ,e x))
                  (declare #.*opt* (,ty* x))
                  ,(format nil "init ~a array with ~d elements." (arrtype ty*) e)
                  (,(veqsymb 1 ty* :_) (list x)))))))
(define-arr-shape 1 :$point) (define-arr-shape 2 :$line)
; (define-arr-shape 3 :$tri) (define-arr-shape 4 :$rect)

