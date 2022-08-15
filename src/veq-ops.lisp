
(in-package :veq)

(defparameter *errmsg* "~%-------------~% error in ~a:~&~a~%-------------~%")
(declaim (list *symbols-map* *docstring-map*))
(defvar *symbols-map* (list))


(defun map-symbol (pair)
  (declare #.*opt* (list pair))
  "add pair macrolet pair. see macro.lisp."
  (export (the symbol (car pair)))
  (setf *symbols-map*
        (remove-if (lambda (cand) (eq (car cand) (car pair))) *symbols-map*))
  (push pair *symbols-map*))

(defun optype (symb)
  (declare #.*opt*)
  "use first letter to select type d -> df, f -> ff."
  (cdr (assoc (char (string-upcase (mkstr symb)) 0)
              `((#\D . df) (#\F . ff) (#\I . in)))))

(defun body-len (n a) (and (= n (length a)) (every #'atom a)))



(defun -expand-!symb (s)
  (declare (symbol s))
  "t if symbol starts with Fd! where d is a positive integer"
  (let ((sn (symbol-name s)))
    (if (and (> (length (symbol-name s)) 2)
             (string= sn "!" :start1 1 :end1 2))
        (loop with rst = (subseq sn 2)
              repeat (reread (char sn 0))
              for s in '(#\X #\Y #\Z #\W #\P #\Q #\U #\V)
              collect (symb rst s))
        s)))

(defun make-broadcast-name (n &aux (n (symbol-name n)))
  (if (numberp (reread (char n 1)))
      (symb (subseq n 0 2) #\$ (subseq n 2))
      (symb (subseq n 0 1) #\$ (subseq n 1))))

(defun -expand-and-flatten-!symbols (ss)
  (awf (loop for s in ss collect (-expand-!symb s))))

(defun -get-!arrdim (args)
  (let ((d (reread (char (symbol-name (car args)) 0))))
    (typecase d (number d) (t 1))))

(defmacro op ((type out-dim mname args) &body body)
  (declare (symbol mname) (list args))
  "build an op. see ops-1.lisp, ops-2.lisp, ..."
  (let* ((exp-args (-expand-and-flatten-!symbols args))
         (declares `(,(optype mname) ,@exp-args))
         (arr-dim (-get-!arrdim args))
         (br-dim (- (length exp-args) arr-dim))
         (fname (symb #\- mname))
         (bname (make-broadcast-name mname))
         (bname! (symb bname "!"))
         (mdocs (format nil "veq context op: ~a
fxname: ~a
args: ~a~%body (~a): ~a." mname fname exp-args out-dim (car body)))
         (bdocs (format nil "veq context broadcast op: ~a
fxname: ~a
args: ~a~%body (~a): ~a." bname fname exp-args out-dim (car body))))
    `(progn (export ',mname)
            (map-symbol `(,',mname (&body mbody)
                            `(,@(if (body-len ,,(length exp-args) mbody)
                                  `(,',',fname)
                                  `(mvc #',',',fname))
                               ,@mbody)))
            (map-docstring ',mname ,mdocs :nodesc :context)

            (export ',bname)
            (map-symbol `(,',bname (a &body mbody)
                           (broadcast-op ,,arr-dim ,,br-dim ',',type ',',fname
                             a mbody :out ,,out-dim)))
            (map-docstring ',bname ,bdocs :nodesc :context)

            ,@(when (= arr-dim out-dim)
                `((export ',bname!)
                  (map-symbol `(,',bname! (a &body mbody)
                    (broadcast-op ,,arr-dim ,,br-dim ',',type ',',fname a mbody)))
                  (map-docstring ',bname! ,(format nil "~a~%destructive." bdocs) :nodesc :context)))
            ,@(unless #.*dev* `((declaim (inline ,fname))))
            (defun ,fname ,exp-args
              (declare ,*opt* ,declares)
              (progn ,@body)))))

(defun -placeholders (root type)
  (labels ((repl (symb type)
            (intern (substitute type #\@
                      (string-upcase (mkstr symb))))))
    (cond ((numberp root) (coerce root (optype type)))
          ((keywordp root) (reread (mkstr root)))
          ((symbolp root) (repl root type))
          ((atom root) root)
          (t (cons (-placeholders (car root) type)
                   (-placeholders (cdr root) type))))))

(defmacro ops (&body body)
  "used to build ops in ops-1.lisp, ops-2.lisp, ..."
  `(progn
     #-:veq-disable-macrolet-singles
     ,@(loop for (o body) in (group (-placeholders body #\F) 2)
             collect `(op (ff ,@o) ,body))
     #-:veq-disable-macrolet-doubles
     ,@(loop for (o body) in (group (-placeholders body #\D) 2)
             collect `(op (df ,@o) ,body))))

