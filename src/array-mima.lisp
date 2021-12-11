
(in-package :veq)

; TODO: type template. argmax/min
; TODO: combine into single function?

(defun mima (a n inds &key type)
  (when (and n inds)
    (error "error in mima: must use either :n or :inds. got both."))
  (awg (a* i minx maxx)
    `(loop with ,a* = ,a
           ,@(cond (n `(for ,i of-type pos-int from 0 below ,n))
                   (inds `(for ,i of-type pos-int in ,inds))
                   (t `(for ,i of-type pos-int from 0 below ($num ,a*))))
           minimizing (aref ,a* ,i) into ,minx of-type ,type
           maximizing (aref ,a* ,i) into ,maxx of-type ,type
           finally (return (values ,minx ,maxx)))))

(defun 2mima (a n inds &key type)
  (when (and n inds)
    (error "error in 2mima: must use either :n or :inds. got both."))
  (awg (a* i* i minx maxx miny maxy)
    `(loop with ,a* = ,a
           ,@(cond (n `(for ,i of-type pos-int from 0 below (* 2 ,n) by 2))
                   (inds `(with ,i of-type pos-int = 0
                           for ,i* of-type pos-int in ,inds
                           do (setf ,i (* 2 ,i*))))
                   (t `(for ,i of-type pos-int from 0 below (* 2 (2$num ,a*)) by 2)))
           minimizing (aref ,a* ,i) into ,minx of-type ,type
           maximizing (aref ,a* ,i) into ,maxx of-type ,type
           minimizing (aref ,a* (1+ ,i)) into ,miny of-type ,type
           maximizing (aref ,a* (1+ ,i)) into ,maxy of-type ,type
           finally (return (values ,minx ,maxx ,miny ,maxy)))))

(defun 3mima (a n inds &key type)
  (when (and n inds)
    (error "error in 3mima: must use either :n or :inds. got both."))
  (awg (a* i* i minx maxx miny maxy minz maxz)
    `(loop with ,a* = ,a
           ,@(cond (n `(for ,i of-type pos-int from 0 below (* 3 ,n) by 3))
                   (inds `(with ,i of-type pos-int = 0
                           for ,i* of-type pos-int in ,inds
                           do (setf ,i (* 3 ,i*))))
                   (t `(for ,i of-type pos-int from 0 below (* 3 (3$num ,a*)) by 3)))
           minimizing (aref ,a* ,i) into ,minx of-type ,type
           maximizing (aref ,a* ,i) into ,maxx of-type ,type
           minimizing (aref ,a* (1+ ,i)) into ,miny of-type ,type
           maximizing (aref ,a* (1+ ,i)) into ,maxy of-type ,type
           minimizing (aref ,a* (+ 2 ,i)) into ,minz of-type ,type
           maximizing (aref ,a* (+ 2 ,i)) into ,maxz of-type ,type
           finally (return (values ,minx ,maxx ,miny ,maxy ,minz ,maxz)))))

