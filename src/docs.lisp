
(in-package :veq)

(declaim (list *docstring-map*))
(defvar *docstring-map* (list))


(defmacro context? ()
  "list all macrolet symbols (ie. ops available inside vprog, fvprogn, vdef,
fvdef defined contexts/functions) and corresponding macro body in veq
context."
  (awg (s) `(sort (mapcar (lambda (,s) ,s) *symbols-map*)
                  #'string-lessp :key #'car)))

(defun desc (sym)
  (declare (symbol sym))
  (let ((d (with-output-to-string (*standard-output*)
             (describe sym))))
    (apply #'mkstr (mapcar (lambda (s) (mkstr " ; " s #\Newline))
                           (butlast (split-string #\Newline d))))))

(defun docstrings (sym)
  (apply #'mkstr
         (mapcar (lambda (o) (mkstr o #\Newline))
                 (remove-if-not #'identity
                                (list (documentation sym 'function)
                                      (documentation sym 'setf))))))

(defun select-docs (sym)
  (declare (symbol sym))
  (let* ((docs (find-if (lambda (c) (eq sym c)) *docstring-map* :key #'car))
         (idocs (docstrings sym))
         (skip (find :skip docs))
         (context (find :context docs))
         (desc (unless (find :nodesc docs) (desc sym))))
    (declare (list docs))

    (values
      (cond (docs (format nil "```~%~a~@[~&~%~a~&~]~&```" (cadr docs) desc))
            ((and idocs (> (length idocs) 0))
              (format nil "```~%~a~@[~&~%~a~&~]~&```" desc nil)) ; nil
            (t (format nil "```~%:none:~%~@[~&~%~a~&~]~&```" desc)))
      skip context)))

(defmacro pckgs ()
  (awg (sym)
    `(sort (loop for ,sym being the external-symbols of (find-package :veq)
                 collect (list (mkstr ,sym) ,sym))
           #'string-lessp :key #'car)))

; TODO: sanitize for _, eg d_ ?
(defun -md-sanitize (d)
  (let ((sp (split-string #\* d)))
    (apply #'veq::mkstr
      (concatenate 'list (mapcar (lambda (s) (veq::mkstr s #\\ #\*))
                                 (butlast sp))
                   (last sp)))))

(defmacro ext-symbols? (&optional mode)
  "list all external symbols in veq. use :verbose to inlcude docstring.
use :pretty to print verbose output to stdout in a readable form."
  (awg (str sym doc skip context)
    (case mode
      (:pretty
        `(loop for (,str ,sym) in (pckgs)
               for (,doc ,skip ,context) = (multiple-value-list (select-docs ,sym))
               if (not ,skip)
               do (format t "~&#### ~:[~;:fvprogn: ~]~a~%~%~a~&~%"
                            ,context (-md-sanitize ,str) ,doc)))
      (:pairs `(loop for (,str ,sym) in (pckgs)
                     collect (list ,str (select-docs ,sym))))
      (otherwise `(loop for (,str ,sym) in (pckgs) collect ,str)))))

(defun map-docstring (&rest rest)
  (declare (list rest))
  "register docs info associated with symbol (car rest). internal."
  (setf *docstring-map* (remove-if (lambda (cand) (eq (car cand) (car rest)))
                                   *docstring-map*))
  (push rest *docstring-map*))

