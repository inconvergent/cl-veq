
(in-package :veq)

; TODO: make general macro similar to array-broadcast

(vdef f$sum (a &aux (n ($num a)))
  (let ((res 0f0))
    (fwith-arrays (:n n :itr k
      :arr ((a 1 a))
      :fxs ((vsum (x) (f+ res x)))
      :exs ((nil k (vsum a)))))
    res))

(vdef f2$sum (a &aux (n (2$num a)))
  (f2let ((res (f2rep 0f0)))
    (fwith-arrays (:n n :itr k
      :arr ((a 2 a))
      :fxs ((vsum (x y) (f2+ res x y)))
      :exs ((nil k (vsum a)))))
    (f2 res)))

(vdef f3$sum (a &aux (n (3$num a)))
  (f3let ((res (f3rep 0f0)))
    (fwith-arrays (:n n :itr k
      :arr ((a 3 a))
      :fxs ((vsum (x y z) (f3+ res x y z)))
      :exs ((nil k (vsum a)))))
    (f3 res)))

(vdef d$sum (a &aux (n ($num a)))
  (let ((res 0d0))
    (dwith-arrays (:n n :itr k
      :arr ((a 1 a))
      :fxs ((vsum (x) (d+ res x)))
      :exs ((nil k (vsum a)))))
    res))

(vdef d2$sum (a &aux (n (2$num a)))
  (d2let ((res (d2rep 0d0)))
    (dwith-arrays (:n n :itr k
      :arr ((a 2 a))
      :fxs ((vsum (x y) (d2+ res x y)))
      :exs ((nil k (vsum a)))))
    (d2 res)))

(vdef d3$sum (a &aux (n (3$num a)))
  (d3let ((res (d3rep 0d0)))
    (dwith-arrays (:n n :itr k
      :arr ((a 3 a))
      :fxs ((vsum (x y z) (d3+ res x y z)))
      :exs ((nil k (vsum a)))))
    (d3 res)))

