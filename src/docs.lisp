(in-package :veq)

(declaim (list *docstring-map*))
(defvar *docstring-map* (list))

(defmacro -outstr (body) `(with-output-to-string (*standard-output*) ,body))
(defun -strsrt (l) (sort l #'string-lessp :key #'car))

(defmacro context? ()
  "list all macrolet symbols (ie. ops available inside vprog, fvprogn, vdef,
fvdef defined contexts/functions) and corresponding macro body in veq
context."
  (awg (s) `(-strsrt (mapcar (lambda (,s) ,s) *symbols-map*))))

(defun desc (sym) (declare #.*opt* (symbol sym))
  (apply #'mkstr (mapcar (lambda (s) (format nil " ; ~a~%" s))
                         (butlast (split-string #\Newline (-outstr (describe sym)))))))
(defun docstrings (sym)
  (apply #'mkstr (mapcar (lambda (o) (mkstr o #\Newline))
                   (remove-if-not #'identity (list (documentation sym 'function)
                                                   (documentation sym 'setf))))))

(defun select-docs (sym) (declare (symbol sym))
  (let* ((docs (find-if (lambda (c) (eq sym c)) *docstring-map* :key #'car))
         (idocs (docstrings sym))
         (skip (find :skip docs))
         (context (find :context docs))
         (desc (unless (find :nodesc docs) (desc sym))))
    (values
      (cond (docs (format nil "~&~a~@[~&~%~a~&~]~&" (cadr docs) desc))
            ((and idocs (> (length idocs) 0))
                  (format nil "~&~a~@[~&~%~a~&~]~&" desc nil)) ; nil
            (t (format nil "~&:missing:~%~@[~&~%~a~&~]~&" desc)))
      skip context)))

(defmacro pckgs (pkg)
  (awg (sym) `(-strsrt (loop for ,sym being the external-symbols of (find-package ,pkg)
                             collect (list (mkstr ,sym) ,sym)))))

(defmacro ext-symbols? (pkg &optional mode (fltfx 'and))
  "list all external symbols in pkg. use :verbose to inlcude docstring.
use :pretty to print verbose output to stdout in a readable form."
  (awg (str sym doc skip context)
    (case mode
      (:pretty
        `(loop for (,str ,sym) in (pckgs ,pkg)
               for (,doc ,skip ,context) = (lst (select-docs ,sym))
               if (and (not ,skip) (,fltfx ,sym ,context))
               do (mvb (,doc ,skip) (select-docs ,sym)
                    (unless ,skip (format t "## `~(~a~)`~&```~&~a~&```~&~%" ,str ,doc)))))
      (:pairs `(loop for (,str ,sym) in (pckgs ,pkg)
                     collect (list ,str (select-docs ,sym))))
      (otherwise `(loop for (,str ,sym) in (pckgs ,pkg) collect ,str)))))

(defun map-docstring (&rest rest) (declare (list rest))
  "register docs info associated with symbol (car rest). internal."
  (setf *docstring-map* (remove-if (lambda (cand) (eq (car cand) (car rest)))
                                   *docstring-map*))
  (push rest *docstring-map*))

