
(in-package :veq)

; TODO: type/dim template

(declaim (inline f2$rect))
(vdef f2$rect (w h)
  (declare #.*opt* (ff w h))
  (f$_ (list (list w (- h)) (list w h)
             (list (- w) h) (list (- w) (- h)))))

(declaim (inline f2$square))
(vdef f2$square (s)
  (declare #.*opt* (ff s))
  (f2$rect s s))


(declaim (inline f2$polygon))
(vdef f2$polygon (n rad &key (rot 0f0))
  (declare #.*opt* (fixnum n) (ff rad rot))
  (f$_ (loop with pin of-type ff = (/ fpii n)
             for i of-type fixnum from 0 below n
             collect (lst (f2scale (fcos-sin (+ rot (ff (* i pin)))) rad)))))

