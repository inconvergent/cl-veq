
(in-package :veq)

(declaim (single-float *eps*) (boolean *dev*) (cons *opt*))

(defparameter *eps* (* 3f0 single-float-epsilon))


; from: http://cl-cookbook.sourceforge.net/os.html
(defun vgetenv (name &optional default)
  #+CMU (let ((x (assoc name ext:*environment-list* :test #'string=)))
          (if x (cdr x) default))
  #-CMU (or #+Allegro (sys:getenv name)
            #+CLISP (ext:getenv name)
            #+ECL (si:getenv name)
            #+SBCL (sb-unix::posix-getenv name)
            #+LISPWORKS (lispworks:environment-variable name)
            default))

(if (> (length (string-downcase (vgetenv "DEV" ""))) 0)
  (progn
    (defparameter *dev* t)
    (defparameter *opt* '(optimize safety (speed 1) debug (space 2)))
    (format t "~&---------!!!!! VEQ DEVMODE !!!!!---------~%"))
  (progn
    (defparameter *dev* nil)
    (defparameter *opt* '(optimize (safety 1) speed (debug 2) space))))
