(in-package :veq)


(fvdef* f2segdst ((varg 2 va vb v))
  "find distance between line, (va vb), and v.
returns (values distance s) where is is the interpolation value that will
yield the closest point on line."
  (declare #.*opt* (ff va vb v))
  (let ((l2 (f2dst2 va vb)))
    (declare (ff l2))
    (if (< l2 *eps*)
      (values (f2dst va v) 0d0) ; line is a point, else:
      (let ((s (max 0f0 (min 1f0 (/ (+ (* (- (vref v 0) (vref va 0))
                                          (- (vref vb 0) (vref va 0)))
                                       (* (- (vref v 1) (vref va 1))
                                          (- (vref vb 1) (vref va 1))))
                                    l2)))))
        (values (f2dst v (f2lerp va vb s)) s)))))


(fvdef* f2segx ((varg 2 a1 a2 b1 b2))
  (declare #.*opt* (ff a1 a2 b1 b2))
  "find intersection between lines (a1 a2), (b1 b2).
returns isect? p q where p and q is the distance along each line to the
intersection point"
  (f2let ((sa (f2!@- a2 a1))
          (sb (f2!@- b2 b1)))
    (let ((u (f2cross sa sb))
          (1eps (- 1f0 *eps*)))
      (declare (ff u 1eps))
      (if (<= (abs u) *eps*)
          ; return nil if the lines are parallel or very short.
          ; this is just a div0 guard. it's not a good way to test.
          (values nil 0f0 0f0)
          ; otherwise check if they intersect
          (f2let ((ab (f2!@- a1 b1)))
            ; t if intersection, nil otherwise
            (let ((p (/ (f2cross sa ab) u))
                  (q (/ (f2cross sb ab) u)))
              (declare (ff p q))
              (values (and (> 1eps p *eps*)
                           (> 1eps q *eps*))
                      q p)))))))


(deftype array-fvec () `(simple-array fvec))

(declaim (inline -sweep-line))
(defun -sweep-line (lines line-points)
  (declare #.*opt* (array-fvec lines) (list line-points))
  "perform sweep line search for intersections along x"
  ; add first line index to sweep line state,
  ; and set sweep line position
  ; TODO: special cases: equal x pos, vertical line
  ; TODO: some related notes on improvements:
  ;       https://en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm
  (let ((res (make-array (length lines)
               :element-type 'list :initial-element nil :adjustable nil))
        (state (list (cdar line-points))))
    (declare (type (simple-array list) res) (list state))

    (labels
      ((-append (i c p)
         (declare #.*opt* (pn i c) (ff p))
         (if (aref res i) (push `(,c . ,p) (aref res i))
                          (setf (aref res i) `((,c . ,p)))))
       (-isects (i cands)
         (declare #.*opt* (pn i) (list cands))
         "intersection test"
         (loop with line of-type fvec = (aref lines i)
               for c of-type pn in cands
               do (fvprogn (mvb (x p q) (f2segx (f2$ line 0 1)
                                                (f2$ (aref lines c) 0 1))
                             (declare (boolean x) (ff p q))
                             (when x (-append i c p) (-append c i q))))))
       (-remove (i)
         (declare #.*opt* (pn i))
         (setf state (remove-if #'(lambda (e)
                                    (declare (optimize speed) (pn e))
                                    (eql e i))
                                state))))

      (loop for (_ . i) of-type (ff . pn) in (cdr line-points)
            ; if i in state, kick i out of state,
            if (member i state) do (-remove i)
            ; else check i against all state, add i to state
            else do (-isects i state) (setf state (cons i state))))
    res))

(declaim (inline -sorted-point-pairs))
(defun -sorted-point-pairs (lines &aux (res (list)))
  (declare #.*opt* (list res) (array-fvec lines))
  (loop for line of-type fvec across lines
        for i of-type pn from 0
        do (push `(,(aref line 0) . ,i) res)
           (push `(,(aref line 2) . ,i) res))
  (sort res #'< :key #'car))


(fvdef* f2lsegx (lines*)
  (declare #.*opt* (sequence lines*))
  "lines = #( #(ax ay bx by) ... )

not entirely slow line-line intersection for all lines. this is faster than
comparing all lines when lines are short relative to the area that the lines
cover. it can be improved further by using binary search tree to store
current state."
  (let ((lines (typecase lines*
                 (list (make-array (length lines*) :initial-contents lines*
                                                   :adjustable nil
                                                   :element-type 'fvec))
                 (vector lines*)
                 (t (error "f2lsegx error: incorrect type: ~a~%" lines*)))))
    (declare (array-fvec lines))
    (-sweep-line lines (-sorted-point-pairs lines))))

;;;;;;;;;; CONCAVE SHAPE RAY CAST TEST

(fvdef* f2in-bbox ((varg 2 top-left bottom-right pt))
  (declare (ff top-left bottom-right pt))
  (and (< (vref top-left 0) (vref pt 0) (vref bottom-right 0))
       (< (vref top-left 1) (vref pt 1) (vref bottom-right 1))))


(fvdef* f2in-concave (shape (varg 2 pt))
  (declare #.*opt* (fvec shape) (ff pt))
  (let ((n (2$num shape)))
    (mvb (minx maxx miny maxy) (f2$mima shape :n n)
      (declare (ff minx maxx miny maxy))
      (unless (f2in-bbox minx miny maxx maxy pt) ; pt outside bbox -> outside shape
              (return-from f2in-concave nil))
      (xlet ((i!c 0) (f!px (- (:vr pt 0) (* 2f0 (- maxx minx)))))
        (f2x@$concave-row-test shape
          ((i (:va 2 row)) (when (f2segx pt px (:vr pt 1) row
                                   (f2$ shape (mod (1+ i) n)))
                             (incf c))))
        (oddp c))))) ; odd number of isects means pt is inside shape


(fvdef* f3planex ((varg 3 n p a b))
  (declare #.*opt* (ff n p a b))
  "intersection of plane (n:normal, p:point) and line (a b)"
  (f3let ((ln (f3!@-  b a)))
    (let ((ldotn (f3dot ln n)))
      (declare (ff ldotn))
      (when (< (abs ldotn) *eps*) ; avoid div0.
            (return-from %f3planex (values nil 0f0 0f0 0f0 0f0))) ; else:
      (let ((d (/ (f3dot (f3!@- p a) n) ldotn)))
        (declare (ff d))
        (~ t d (f3from a ln d))))))

; TODO: this
; (defun f2inside-convex-poly (convex v)
;   (declare #.*opt-settings* (list convex) (vec v))
;   (loop with convex* = (math:close-path* convex)
;         for a of-type vec in convex*
;         and b of-type vec in (cdr convex*)
;         always (>= (cross (sub b a) (sub v b)) 0d0)))

(fvdef* f2in-triangle ((varg 2 a b c p))
  (declare #.*opt* (ff a b c p))
  (labels ((f2norm-safe ((varg 2 a))
             (declare (ff a))
             (let ((l (f2len a)))
               (declare (ff l))
               (if (< *eps* l) (f2iscale a l) (f2rep 0f0))))
           (check ((varg 2 p1 p2 p3))
            (declare (ff p1 p2 p3))
            (f2cross (mvc #'f2norm-safe (f2!@- p3 p1))
                     (mvc #'f2norm-safe (f2!@- p3 p2)))))
    (let ((d1 (check a b p)) (d2 (check b c p)) (d3 (check c a p))
          (ep (- *eps*)) (-ep (+ *eps*)))
      (not (and (or (< d1 ep) (< d2 ep) (< d3 ep))
                (or (> d1 -ep) (> d2 -ep) (> d3 -ep)))))))

