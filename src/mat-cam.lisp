
(in-package :veq)

; TODO: double float

; http://www.songho.ca/opengl/gl_projectionmatrix.html

(export 'fmake-proj-matrix)
(veq:fvdef* fmake-proj-matrix (&optional (w 1f0) (h w) (n 0.1) (f 100f0))
  (declare (ff w h n f))
  "make projection matrix for width, height, near, far "
  (let ((right (* -0.5 w))
        (top (* -0.5 h))
        (2n (* 2 n)))
    (declare (ff right top 2n))
    (veq:f_ (list (/ n right) 0f0 0f0 0f0
                  0f0 (/ n top) 0f0 0f0
                  0f0 0f0 (- (/ (+ f n) (- f n))) (- (/ (* f 2n) (- f n)))
                  0d0 0f0 -1f0 0f0))))

(export 'fmake-ortho-proj-matrix)
(veq:fvdef* fmake-ortho-proj-matrix (&optional (w 1f0) (h w) (n 0.1) (f 100f0))
  (declare (ff w h n f))
  "make orthogonal projection matrix"
  (let ((right (* -0.5 w))
        (top (* -0.5 h)))
    (declare (ff right top))
    (veq:f_ (list (/ right) 0f0 0f0 0f0
                  0f0 (/ top) 0f0 0f0
                  0f0 0f0 (/ -2f0 (- f n)) (- (/ (+ f n) (- f n)))
                  0d0 0f0 0f0 1f0))))

(export 'fmake-view-matrix)
(veq:fvdef* fmake-view-matrix ((:va 3 cam target up))
  (declare (ff cam target up))
  "make view matrix for cam (w/up) looking at target"
  (veq:f3let ((za (veq:f3norm (veq:f3- cam target)))
              (xa (veq:f3norm (veq:f3cross up za)))
              (ya (veq:f3norm (veq:f3cross za xa))))
    ; (veq:f_ (list (:vr xa 0) (:vr ya 0) (:vr za 0) 0f0
    ;               (:vr xa 1) (:vr ya 1) (:vr za 1) 0f0
    ;               (:vr xa 2) (:vr ya 2) (:vr za 2) 0f0
    ;               (- (veq:f3. xa cam)) (- (veq:f3. ya cam)) (- (veq:f3. za cam)) 1f0))
    (veq:f_ (list (:vr xa 0) (:vr xa 1) (:vr xa 2) (- (veq:f3. xa cam))
                  (:vr ya 0) (:vr ya 1) (:vr ya 2) (- (veq:f3. ya cam))
                  (:vr za 0) (:vr za 1) (:vr za 2) (- (veq:f3. za cam))
                  0f0 0f0 0f0 1f0))))

