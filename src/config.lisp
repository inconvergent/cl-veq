(in-package :veq)

(declaim (single-float *eps*) (boolean *dev*) (cons *opt*))

(defparameter *eps* #.(* 1f0 single-float-epsilon))

(init-config (optimize safety (speed 1) debug (space 2))
             (optimize (safety 1) (speed 3) (debug 1) (space 2)))

(defun v? (&optional (silent t))
  "get version. use silent to surpress stdout"
  (let ((v (slot-value (asdf:find-system 'veq) 'asdf:version)))
    (unless silent (format t "~%veq version: ~a~%" v))
    v))
(defun d? (f) "describe argument" (describe f))
(defun i? (f) "inspect argument" (inspect f))

; runtime / compile time
(declaim (boolean *silent-rt* *silent-ct*))
(defvar *silent* nil) (defvar *silent-rt* nil) (defvar *silent-ct* nil)

(defmacro silent? (&optional (mode :rt) &rest rest)
  `(unless (or *silent* ,(ecase mode (:rt *silent-rt*)
                                     (:ct *silent-ct*)))
           (progn ,@rest)))

