
(in-package :veq)


(defmacro make-swizzle-macro (symb type)
  (awg (rest)
    (let* ((string-symb (string-upcase (mkstr symb)))
           (exportname (veqsymb 1 type string-symb))
           (symb (map 'list #'symb string-symb))
           (vals `(values ,@symb))
           (ign `(ignore ,@(set-difference '(x y z w) symb)))
           (typ `(,type ,@symb))
           (docs (format nil "macro. reorder arguments (X Y Z W) as ~a, ~a.~%" symb ign)))
      `(progn (export ',exportname)
              (map-docstring ',exportname ,docs :skip)
              (defmacro ,exportname (&rest ,rest)
                ,docs
                `(mvb (x y z w) (mvc #'values ,@,rest)
                   (declare ,',typ ,',ign)
                   ,',vals))))))


(defmacro make-swizzle (type)
  "
  build all selector macros for combinations of xyzw. so that
  (xyxw (values 1f0 2f0 3f0 4f0)) -> (values 1f0 2f0 1f0 4f0).
  "
  (labels
    ((rec-swizzle-gen (symbs &aux (res))
       (labels ((acc (a &optional (b "")) (push* (symb a b) res)) ; -> (symb a b)
                (rec (res level)
                  (when (> level 2) (return-from rec))
                  (loop for s in res
                        do (rec (mapcar (lambda (b) (acc s b)) symbs)
                                (1+ level)))))
         (rec (loop for s in symbs collect (acc s)) 0)
         res)))

    `(progn ,@(loop for symb in (rec-swizzle-gen '(x y z w))
                    collect `(make-swizzle-macro ,symb ,type)))))

(make-swizzle ff)
(make-swizzle df)

; (defun id (&rest rest) (apply #'values rest))
; (defun idx (x &rest rest) x)
; (defun idxy (x y &rest rest) x y)
; (defun idxyz (x y &rest rest) x y z)

