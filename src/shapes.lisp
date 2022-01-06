
(in-package :veq)

; TODO: type/dim template?

(vdef* f2$rect (w h)
  (declare #.*opt* (ff w h))
  (f$_ `((,w ,(- h)) (,w ,h) (,(- w) ,h) (,(- w) ,(- h)))))
(vdef* f2$square (s) (declare #.*opt* (ff s)) (f2$rect s s))

(vdef* f2$polygon (n rad &key (rot 0f0))
  (declare #.*opt* (fixnum n) (ff rad rot))
  (f$_ (loop with pin of-type ff = (/ fpii n)
             for i of-type fixnum from 0 below n
             collect (lst (f2scale (fcos-sin (+ rot (ff (* i pin)))) rad)))))

(vdef* f2$circ (rad &key (rs 0.5f0))
  (declare #.*opt* (ff rad rs))
  (f2$polygon (values (ceiling (the ff (* fpii (the ff (* rad rs)))))) rad))


(vdef* f$point (a) (declare #.*opt* (ff a)) (f_ (list a)))
(vdef* f2$point ((varg 2 x)) (declare #.*opt* (ff x)) (f_ (list x)))
(vdef* f3$point ((varg 3 x)) (declare #.*opt* (ff x)) (f_ (list x)))

(vdef* f$line ((varg 2 x)) (declare #.*opt* (ff x)) (f_ (list x)))
(vdef* f2$line ((varg 4 x)) (declare #.*opt* (ff x)) (f_ (list x)))
(vdef* f3$line ((varg 6 x)) (declare #.*opt* (ff x)) (f_ (list x)))

(vdef* d$point (a) (declare #.*opt* (df a)) (d_ (list a)))
(vdef* d2$point ((varg 2 x)) (declare #.*opt* (df x)) (d_ (list x)))
(vdef* d3$point ((varg 3 x)) (declare #.*opt* (df x)) (d_ (list x)))

(vdef* d$line ((varg 2 x)) (declare #.*opt* (df x)) (d_ (list x)))
(vdef* d2$line ((varg 4 x)) (declare #.*opt* (df x)) (d_ (list x)))
(vdef* d3$line ((varg 6 x)) (declare #.*opt* (df x)) (d_ (list x)))

(vdef* f2$center (arr &aux (n (2$num arr)))
  (declare #.*opt* (fvec arr) (pos-int n))
  (mvb (minx maxx miny maxy) (f2$mima arr :n n)
       (f2$+ arr (f2 (- (* 0.5 (+ minx maxx)))
                     (- (* 0.5 (+ miny maxy)))))))

