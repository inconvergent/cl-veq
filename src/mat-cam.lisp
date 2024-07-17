
(in-package :veq)

; http://www.songho.ca/opengl/gl_projectionmatrix.html

(declaim (inline fmake-proj-matrix fmake-ortho-proj-matrix fmake-view-matrix))

(fvdef fmake-proj-matrix (&optional (w 1f0) (h w) (n 0.1) (f 100f0))
  (declare #.*opt* (ff w h n f)) "make projection matrix for width, height, near, far "

  (xlet ((f!rr (* -0.5 w))
         (f!tt (* -0.5 h))
         (f!2n (*  2.0 n)))

    (f$~ (16)  (/ n rr)  0f0       0f0                      0f0
               0f0       (/ n tt)  0f0                      0f0
               0f0       0f0       (- (/ (+ f n) (- f n)))  (- (/ (* f 2n) (- f n)))
               0f0       0f0       -1f0                     0f0)))

(fvdef fmake-ortho-proj-matrix (&optional (w 1f0) (h w) (n 0.1) (f 100f0))
  (declare #.*opt* (ff w h n f)) "make orthogonal projection matrix"

  (xlet ((f!rr (* -0.5 w))
         (f!tt (* -0.5 h)))

    (f$~ (16)  (/ rr)  0f0     0f0               0f0
               0f0     (/ tt)  0f0               0f0
               0f0     0f0     (/ -2f0 (- f n))  (- (/ (+ f n) (- f n)))
               0f0     0f0     0f0               1f0)))

(fvdef fmake-view-matrix ((:va 3 pos look up))
  (declare #.*opt* (ff pos look up)) "make view matrix for pos, up looking at look"

  (f3let ((za (f3norm (f3!@- pos look)))
          (xa (f3norm (f3cross up za)))
          (ya (f3norm (f3cross za xa))))

    (f$~ (16) (:vr xa 0)  (:vr xa 1)  (:vr xa 2)  (- (f3dot xa pos))
              (:vr ya 0)  (:vr ya 1)  (:vr ya 2)  (- (f3dot ya pos))
              (:vr za 0)  (:vr za 1)  (:vr za 2)  (- (f3dot za pos))
              0f0         0f0         0f0         1f0)))

