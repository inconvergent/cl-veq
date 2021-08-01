
(in-package :veq)


; TODO: port this
(declaim (inline f2segdst))
(vdef f2segdst ((varg 2 va vb v))
  "
  find distance between line, (va vb), and v.
  returns (values distance s) where is is the interpolation value that will
  yield the closest point on line.
  "
  (declare #.*opt* (ff va vb v))
  (let ((l2 (f2dst2 va vb)))
    (declare (ff l2))
    (if (< l2 *eps*)
      ; line is a point
      (values (f2dst va v) 0d0)
      ; else
      (let ((s (max 0f0 (min 1f0 (/ (+ (* (- (vref v 0) (vref va 0))
                                          (- (vref vb 0) (vref va 0)))
                                       (* (- (vref v 1) (vref va 1))
                                          (- (vref vb 1) (vref va 1))))
                                    l2)))))
        (values (f2dst v (f2lerp va vb s)) s)))))



(declaim (inline f2segx))
(vdef f2segx ((varg 2 a1 a2 b1 b2))
  (declare #.*opt* (ff a1 a2 b1 b2))
  "
  find intersection between lines (a1 a2), (b1 b2).
  returns isect? p q where p and q is the distance along each line to the
  intersection point
  "
  (f2let ((sa (f2- a2 a1))
          (sb (f2- b2 b1)))

    (let ((u (f2cross sa sb)))
      (declare (ff u))
      (if (<= (abs u) #.*eps*)
          ; return nil if the lines are parallel or very short.
          ; this is just a div0 guard. it's not a good way to test.
          (values nil 0f0 0f0)
          ; otherwise check if they intersect
          (f2let ((ab (f2- a1 b1)))
            ; t if intersection, nil otherwise
            (let ((p (/ (f2cross sa ab) u))
                  (q (/ (f2cross sb ab) u)))
              (declare (ff p q))
              (values (and (> #.(- 1f0 *eps*) p #.*eps*)
                           (> #.(- 1f0 *eps*) q #.*eps*))
                      q p)))))))


(deftype array-fvec () `(simple-array fvec))

(defun -to-array-fvec (l)
  (declare (list l))
  (make-array (length l)
    :initial-contents l :adjustable nil :element-type 'fvec))


(declaim (inline -sweep-line))
(defun -sweep-line (lines line-points)
  (declare #.*opt* (array-fvec lines) (list line-points))
  "perform sweep line search for intersections along x"
  ; add first line index to sweep line state,
  ; and set sweep line position
  ; TODO: special cases: equal x pos, vertical line
  (let ((res (make-array (length lines)
               :element-type 'list :initial-element nil :adjustable nil))
        (state (list (cdar line-points))))
    (declare (type (simple-array list) res) (list state))

    (labels
      ((-append (i c p)
         (declare #.*opt* (pos-int i c) (ff p))
         (if (aref res i) (push `(,c . ,p) (aref res i))
                          (setf (aref res i) `((,c . ,p)))))

       (-isects (i cands)
         (declare #.*opt* (pos-int i) (list cands))
         "intersection test"
         (loop with line of-type fvec = (aref lines i)
               for c of-type pos-int in cands
               do (vprogn (mvb (x p q) (mvc #'f2segx (f2$ line 0 1)
                                                     (f2$ (aref lines c) 0 1))
                            (declare (boolean x) (ff p q))
                            (when x (-append i c p) (-append c i q))))))
       (-remove (i)
          (declare #.*opt* (pos-int i))
          (setf state (remove-if #'(lambda (e)
                                     (declare (optimize speed) (pos-int e))
                                     (eql e i))
                                 state))))

      (loop for (_ . i) of-type (ff . pos-int) in (cdr line-points)
            ; if i in state, kick i out of state,
            if (member i state) do (-remove i)
            ; else check i against all state, add i to state
            else do (-isects i state) (setf state (cons i state))))
    res))

(declaim (inline -sorted-point-pairs))
(defun -sorted-point-pairs (lines &aux (res (list)))
  (declare #.*opt* (list res) (array-fvec lines))
  (loop for line of-type fvec across lines
        for i of-type pos-int from 0
        do (push `(,(aref line 0) . ,i) res)
           (push `(,(aref line 2) . ,i) res))
  (sort res #'< :key #'car))

(defun f2lsegx (lines*)
  (declare #.*opt* (sequence lines*))
  "
  lines = #( #(ax ay bx by) ... )

  not entirely slow line-line intersection for all lines. this is faster than
  comparing all lines when lines are short relative to the area that the lines
  cover. it can be improved further by using binary search tree to store
  current state.
  "
  (let ((lines (if (listp lines*) (-to-array-fvec lines*) lines*)))
    (declare (array-fvec lines))
    (-sweep-line lines (-sorted-point-pairs lines))))

;;;;;;;;;; CONCAVE SHAPE RAY CAST TEST

(vdef f2inside-bbox ((varg 2 top-left bottom-right pt))
  (declare (ff top-left bottom-right pt))
  (and (< (vref top-left 0) (vref pt 0) (vref bottom-right 0))
       (< (vref top-left 1) (vref pt 1) (vref bottom-right 1))))


(vdef f2inside-concave (shape (varg 2 pt))
  (declare (fvec shape) (ff pt))
  (let ((n (round (/ (length shape) 2))))

    (mvb (minx maxx miny maxy) (f2mima n shape)
      ; pt outside bbox -> outside shape
      (unless (f2inside-bbox minx miny maxx maxy pt)
              (return-from f2inside-concave nil))

      (let* ((c 0)
             (width (- maxx minx))
             (shift (- (vref pt 0) (* 2f0 width))))
        (2for-all-rows (n shape)
          (lambda (i (varg 2 a))
            (declare (optimize speed) (ff a))
            (when (mvc #'f2segx pt shift (vref pt 1)
                              a (f2$ shape (mod (1+ i) n)))
                  (incf c))))
        ; odd number of isects means pt is inside shape
        (oddp c)))))



