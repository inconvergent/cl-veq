

(in-package :veq)

(declaim (single-float *eps*) (boolean *dev*) (cons *opt*))

(defparameter *eps* #.(* 1f0 single-float-epsilon))

(init-config (optimize safety (speed 1) debug (space 2))
             (optimize (safety 1) (speed 3) debug space))

