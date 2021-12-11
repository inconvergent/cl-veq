
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
  (f2$polygon (values (the fixnum (ceiling (the ff (* fpii (the ff (* rad rs))))))) rad))

(vdef* f2$center (arr &aux (n (2$num arr)))
  (declare #.*opt* (fvec arr) (pos-int n))
  (mvb (minx maxx miny maxy) (f2$mima n arr)
       (f2$+ arr (f2< (- (* 0.5 (+ minx maxx)))
                      (- (* 0.5 (+ miny maxy)))))))

