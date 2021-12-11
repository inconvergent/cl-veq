
(in-package :veq)


(defmacro mvcwrap (m fx)
  "wrap fx in a macro, m, so that fx will be called via mvc"
  `(defmacro ,m (&rest args)
    `(mvc #',',fx ,@args)))


(mvcwrap vprod *) ; (* a b c)
(mvcwrap vsum +) ; (+ a b c)


(defmacro -vprint (&rest rest)
  "print (mvc #'list rest) and return (mvc #'values rest)"
  (awg (res)
    `(let ((,res (lst ,@rest)))
       (format t "~%val of~%  ~a:~%  ~a~%" ',rest ,res)
       (apply #'values ,res))))
(abbrev vpr -vprint)

