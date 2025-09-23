(in-package :veq)

(declaim (single-float *eps*) (boolean *dev*) (cons *opt*))
(declaim (boolean *silent-rt* *silent-ct*)) ; runtime / compile time
(defparameter *eps* #.(* 1f0 single-float-epsilon))
(defvar *silent* nil) (defvar *silent-rt* nil) (defvar *silent-ct* nil)
(defparameter *srndopt* '(optimize speed (safety 0) (debug 1) (space 3)))

(init-config (optimize safety (speed 1) debug (space 3))
             (optimize (safety 1) (speed 3) (debug 1) (space 3)))

(defun v? (&optional (silent t))
  "get version. use silent to surpress stdout"
  (let ((v (slot-value (asdf:find-system 'veq) 'asdf:version)))
    (unless silent (format t "~%veq version: ~a~%" v))
    v))
(defun d? (f) "describe argument" (describe f))
(defun i? (f) "inspect argument" (inspect f))

(defmacro silent? (&optional (mode :rt) &rest rest)
  `(unless (or *silent* ,(ecase mode (:rt *silent-rt*)
                                     (:ct *silent-ct*)))
           (progn ,@rest)))

(defun internal-path-string (path &optional (pkg :veq)) (declare (string path))
  (namestring (asdf:system-relative-pathname pkg path)))


