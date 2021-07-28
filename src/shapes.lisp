
(in-package :veq)

; TODO: type/dim template

(declaim (inline f2$rect))
(vdef f2$rect (w h)
  (declare #.*opt* (ff w h))
  (f$_ `((,w ,(- h)) (,w ,h) (,(- w) ,h) (,(- w) ,(- h)))))

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

(defun f2$circ (rad &key (rs 0.5f0))
  (declare #.*opt* (ff rad rs))
  (f2$polygon (ceiling (* fpii rad rs)) rad))


(veq:vdef f2$center (arr &aux (n (/ (length arr) 2)))
  (declare #.*opt* (fvec arr) (pos-int n))
  (mvb (minx maxx miny maxy) (f2mima n arr)
       (f2$+ arr (f2< (- (* 0.5 (+ minx maxx)))
                              (- (* 0.5 (+ miny maxy)))))))

