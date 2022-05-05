
(in-package :veq)

; TODO: type/dim template?


(fvdef* f2$rect (w h)
 (declare #.*opt* (ff w h))
 (f$_ `((,w ,(- h)) (,w ,h) (,(- w) ,h) (,(- w) ,(- h)))))
(def* f2$square (s) (declare #.*opt* (ff s)) (f2$rect s s))

(fvdef* f2$polygon (n rad &key (rot 0f0))
 (declare #.*opt* (pos-int n) (ff rad rot))
 "return n-polygon of size rad. rotate by (rot 0)"
 (f$_ (loop with pin of-type ff = (/ fpii n)
            for i of-type pos-int from 0 below n
            collect (lst (f2scale (fcos-sin (+ rot (ff (* i pin)))) rad)))))

(fvdef* f2$circ (rad &key (rs 0.5f0))
 (declare #.*opt* (ff rad rs))
 "return circle of size rad. (rs 0.5) is vertex density."
 (let ((n (ceiling (the ff (* fpii (the ff (* rad rs)))))))
   (declare (pos-int n))
   (f2$polygon n rad)))

(fvdef* f2$center (arr &aux (n (2$num arr)))
  (declare #.*opt* (fvec arr) (pos-int n))
  "center 2d array according to n points in array. n is optional."
  (veq:mvb ((varg 2 xx yy)) (f2$mima arr :n n)
    (f2$+ arr (fmid xx ) (fmid yy))))


(declaim (inline %f$point %f2$point %f3$point %f4$point %f$line %f2$line
                 %f3$line %f4$line %d$point %d2$point %d3$point %d4$point
                 %d$line %d2$line %d3$line %d4$line))

(fvprogn


(def* f$point (a) (declare #.*opt* (ff a)) (f_ (list a)))
(def* f2$point ((varg 2 x)) (declare #.*opt* (ff x)) (f_ (list x)))
(def* f3$point ((varg 3 x)) (declare #.*opt* (ff x)) (f_ (list x)))
(def* f4$point ((varg 4 x)) (declare #.*opt* (ff x)) (f_ (list x)))

(def* f$line ((varg 2 x)) (declare #.*opt* (ff x)) (f_ (list x)))
(def* f2$line ((varg 4 x)) (declare #.*opt* (ff x)) (f_ (list x)))
(def* f3$line ((varg 6 x)) (declare #.*opt* (ff x)) (f_ (list x)))
(def* f4$line ((varg 6 x)) (declare #.*opt* (ff x)) (f_ (list x)))

(def* d$point (a) (declare #.*opt* (df a)) (d_ (list a)))
(def* d2$point ((varg 2 x)) (declare #.*opt* (df x)) (d_ (list x)))
(def* d3$point ((varg 3 x)) (declare #.*opt* (df x)) (d_ (list x)))
(def* d4$point ((varg 4 x)) (declare #.*opt* (df x)) (d_ (list x)))

(def* d$line ((varg 2 x)) (declare #.*opt* (df x)) (d_ (list x)))
(def* d2$line ((varg 4 x)) (declare #.*opt* (df x)) (d_ (list x)))
(def* d3$line ((varg 6 x)) (declare #.*opt* (df x)) (d_ (list x)))
(def* d4$line ((varg 6 x)) (declare #.*opt* (df x)) (d_ (list x)))

  )
