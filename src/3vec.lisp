
(declaim (inline 3xy 3yx 3xz 3zx 3yz 3zy))
(defun 3xy (v) (declare #.*opt-settings* (3vec v)) (vec (3vec-x v) (3vec-y v)))
(defun 3yx (v) (declare #.*opt-settings* (3vec v)) (vec (3vec-y v) (3vec-x v)))
(defun 3xz (v) (declare #.*opt-settings* (3vec v)) (vec (3vec-x v) (3vec-z v)))
(defun 3zx (v) (declare #.*opt-settings* (3vec v)) (vec (3vec-z v) (3vec-x v)))
(defun 3yz (v) (declare #.*opt-settings* (3vec v)) (vec (3vec-y v) (3vec-z v)))
(defun 3zy (v) (declare #.*opt-settings* (3vec v)) (vec (3vec-z v) (3vec-y v)))


(declaim (inline 3refract))
(defun 3refract (i n eta)
  (declare #.*opt-settings* (3vec i n) (double-float eta))
  (let* ((dni (3dot n i))
         (k (- 1d0 (* (* eta eta) (- 1d0 (* dni dni))))))
    (declare (double-float dni k))
    (if (<= k 0d0)
        ; total reflection
        (values *3zero* nil)
        ; refraction
        (values (3sub! (3smult i eta)
                       (3smult n (+ (* eta dni)
                                    (sqrt (the pos-double k)))))
                t))))

(declaim (inline -plane-test))
(defun -plane-test (p l0 n s)
  (declare #.*opt-settings* (3vec p l0 n) (double-float s))
  ; (/ (3dot (3sub p l0) n) s)
  (/ (+ (* (- (3vec-x p) (3vec-x l0)) (3vec-x n))
        (* (- (3vec-y p) (3vec-y l0)) (3vec-y n))
        (* (- (3vec-z p) (3vec-z l0)) (3vec-z n)))
     s))

(defun 3planex (n p line)
  (declare #.*opt-settings* (3vec n p) (list line))
  "intersection of plane (n:normal, p:point) and line"
  (destructuring-bind (l0 l1) line
    (declare (3vec l0 l1))
    (let* ((ln (3sub l1 l0))
           (ldotn (3dot ln n)))
      (declare (3vec ln) (double-float ldotn))
      ; avoid div0.
      (when (< (abs ldotn) 1d-14)
            (return-from 3planex (values nil 0d0 (3zero))))
      ; else
      (let ((d (-plane-test p l0 n ldotn)))
        (declare (double-float d))
        (values t d (3from l0 ln d))))))

;https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
(declaim (inline -3polyx))
(defun -3polyx (v0 e1 e2 org l)
  (declare #.*opt-settings* (3vec org l v0 e1 e2))
  (let* ((h (3cross l e2))
         (a (3dot e1 h)))
    (declare (3vec h) (double-float a))

  ; parallel
  (when (< (abs a) *eps*)
        (return-from -3polyx (values nil 0d0 *3zero*)))

  (let* ((f (/ a))
         (s (3sub org v0))
         (u (* f (3dot s h))))
    (declare (3vec s) (double-float f u))

    (when (or (> u 1d0) (< u 0d0))
          (return-from -3polyx (values nil 0d0 *3zero*)))

    (let* ((q (3cross! s e1))
           (v (* f (3dot l q))))
      (declare (3vec q) (double-float v))
      (when (or (< v 0d0) (> (+ u v) 1d0))
            (return-from -3polyx (values nil 0d0 *3zero*)))

      (let ((tt (* f (3dot e2 q))))
        (declare (double-float tt))
        (if (> 1d0 tt *eps*)
            ; intersection on line
            (values t tt (3from org l tt))
            ; intersection (not on line)
            (values nil tt (3from org l tt))))))))

(defun 3polyx (verts line )
  (declare #.*opt-settings* (list verts line))
  (destructuring-bind (v0 v1 v2) verts
    (declare (3vec v0 v1 v2))
    (-3polyx v0 (3sub v1 v0) (3sub v2 v0)
             (first line) (apply #'3isub line))))


; https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
(declaim (inline -3spherex))
(defun -3spherex (r oc l llen)
  (declare #.*opt-settings* (3vec oc l) (double-float r llen))
  (let* ((ldotoc (3dot l oc))
         (s (- (expt ldotoc 2d0) (- (3len2 oc) (* r r)))))
      (declare (double-float ldotoc s))

      (when (< s 0d0) (return-from -3spherex (values nil nil)))
      (when (> s 0d0) (return-from -3spherex
                        (values t (list (/ (- s ldotoc) llen)
                                        (/ (- (- ldotoc) s) llen)))))
      (values t (list (/ (- ldotoc) llen)))))

(declaim (inline 3spherex))
(defun 3spherex (c r line)
  (declare #.*opt-settings* (3vec c) (double-float r) (list line))
  "intersection of sphere (c, r) and line (o e)"
  (destructuring-bind (o e) line
    (declare (3vec o e))
    (let* ((ll (3sub e o))
           (llen (3len ll)))
      (declare (3vec ll) (double-float llen))
      (-3spherex r (3sub o c) (3sdiv ll llen) llen))))

