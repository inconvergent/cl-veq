
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

(defun f2$circ (rad &key (rs 0.5f0))
  (f2$polygon (ceiling (* fpii rad rs)) rad))


(declaim (inline f2$lvs))
(veq:vdef f2$lvs (a &optional (i 0) (j 1))
  (declare (fvec a) (pos-int i j))
  (mvc #'values (f2> a i) (f2> a j)))

(declaim (inline f3$lvs))
(veq:vdef f3$lvs (a &optional (i 0) (j 1))
  (declare (fvec a) (pos-int i j))
  (mvc #'values (f2> a i) (f2> a j)))

