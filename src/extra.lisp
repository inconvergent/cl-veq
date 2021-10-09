
(in-package :veq)


(defmacro mvcwrap (m fx)
  "wrap fx in a macro, m, so that fx will be called via mvc"
  `(defmacro ,m (&rest args)
    `(mvc #',',fx ,@args)))


(mvcwrap vprod *) ; (* a b c)
(mvcwrap vsum +) ; (+ a b c)


(defmacro -vprint (&rest rest)
  "print (mvc #'list rest) and return (mvc #'values rest)"
  `(progn (format t "~%val of~%  ~a:~%  ~a~%" ',rest (lst ,@rest))
          (mvc #'values ,@rest)))
(abbrev vpr -vprint)

