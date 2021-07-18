
(in-package :veq)


; TODO: port this
; (declaim (inline ptinside))
; (defun ptinside (convex v)
;   (declare #.*opt* (list convex) (vec v))
;   (loop with convex* = (math:close-path* convex)
;         for a of-type vec in convex*
;         and b of-type vec in (cdr convex*)
;         always (>= (cross (sub b a) (sub v b)) 0d0)))


; TODO: port this
; (declaim (inline segdst))
; (defun segdst (line v)
;   "
;   find distance between line and v.
;   returns values (distance s) where is is the interpolation value that will
;   yield the closest point on line.
;   "
;   (declare #.*opt* (list line) (vec v))
;   (destructuring-bind (va vb) line
;     (declare (vec va vb))
;     (let ((l2 (dst2 va vb)))
;       (declare (ff l2))
;       (if (<= l2 0d0)
;         ; line is a point
;         (values (dst va v) 0d0)
;         ; else
;         (let ((tt (/ (+ (* (- (vec-x v) (vec-x va)) (- (vec-x vb) (vec-x va)))
;                         (* (- (vec-y v) (vec-y va)) (- (vec-y vb) (vec-y va))))
;                      l2)))
;           (if (> tt 1d0) (setf tt 1d0))
;           (if (< tt 0d0) (setf tt 0d0))
;           (values (dst v (on-line tt va vb)) tt))))))


(declaim (inline segx))
(vdef f2segx ((varg 2 a1 a2 b1 b2))
  (declare #.*opt* (ff a1 a2 b1 b2))
  "
  find intersection between lines aa, bb.
  returns isect? p q where p and q is the distance along each line to the
  intersection point
  "
  (f2let ((sa (f2- a2 a1))
          (sb (f2- b2 b1)))
    (let ((u (f2cross  sa sb)))
      (declare (ff u))
      (if (<= (abs u) 0.00001f0)
          ; return nil if the lines are parallel (nil)
          ; this is just a div0 guard. it's not a good way to test.
          (values nil 0f0 0f0)
          ; otherwise check if they intersect
          (f2let ((ab (f2-  a1 b1)))
            ; t if intersection, nil otherwise
            (let ((p (/ (f2cross sa ab) u))
                  (q (/ (f2cross sb ab) u)))
              (declare (ff p q))
              (values (and (> p 0f0) (< p 1f0) (> q 0f0) (< q 1f0))
                      q p)))))))


(deftype array-list () `(simple-array veq:fvec))

(defun -line-to-values (l)
  (declare (fvec l))
  (apply #'values (loop for i from 0 below 4 collect (aref l i))))

(defun -to-vector (l)
  (declare (list l))
  (make-array (length l) :initial-contents l
                         :adjustable nil
                         :element-type 'fvec))


(declaim (inline -sweep-line))
(defun -sweep-line (lines line-points)
  (declare #.*opt* (array-list lines) (list line-points))
  "perform sweep line search for intersections along x"
  ; add first line index to sweep line state,
  ; and set sweep line position
  ; TODO: special cases: equal x pos, vertical line
  (let ((res (make-array (length lines) :element-type 'list
                                        :initial-element nil
                                        :adjustable nil))
        (q (list (cdar line-points))))
    (declare (type (simple-array list) res) (list q))

    (labels
      ((-append (i c p)
         (declare (fixnum i c) (ff p))
         (if (aref res i) (push `(,c . ,p) (aref res i))
                          (setf (aref res i) `((,c . ,p)))))

       (-isects (i cands)
         (declare (fixnum i) (list cands))
         "intersection test"
         (loop with line of-type veq:fvec = (aref lines i)
               for c of-type fixnum in cands
               do (mvb (x p qq) (mvc #'f2segx (-line-to-values line)
                                              (-line-to-values (aref lines c)))
                    (when x (-append i c p) (-append c i qq))))))

      (loop for (_ . i) of-type (ff . fixnum) in (cdr line-points)
            ; if i in q, kick i out of q,
            if (member i q) do (setf q (remove-if #'(lambda (e) (eq e i)) q))
            ; else check i against all q, add i to q
            else do (-isects i q) (setf q (cons i q))))
    res))

(declaim (inline -sorted-point-pairs))
(defun -sorted-point-pairs (lines)
  (declare #.*opt* (array-list lines))
  (loop with res of-type list = (list)
        for l of-type fvec across lines
        for i of-type fixnum from 0
        do (push `(,(aref l 0) . ,i) res)
           (push `(,(aref l 2) . ,i) res)
        finally (return (sort res #'< :key #'car))))

(defun f2lsegx (lines*)
  (declare (sequence lines*))
  "

  lines = #( #(ax ay bx by) ... )

  not entirely slow line-line intersection for all lines. this is faster than
  comparing all lines when lines are short relative to the area that the lines
  cover. it can be improved further by using binary search tree to store
  current state.
  "
  (let ((lines (if (listp lines*) (-to-vector lines*) lines*)))
    (declare (array-list lines))
    (-sweep-line lines (-sorted-point-pairs lines))))

