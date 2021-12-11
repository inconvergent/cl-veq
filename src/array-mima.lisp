
(in-package :veq)

; TODO: type template. argmax/min
; TODO: remove minmax*, make n/inds optional.

(defun minmax (a n &key type)
  (awg (i minx maxx)
    `(loop for ,i of-type pos-int from 0 below ,n
           minimizing (aref ,a ,i) into ,minx of-type ,type
           maximizing (aref ,a ,i) into ,maxx of-type ,type
           finally (return (values ,minx ,maxx)))))

(defun minmax* (a inds &key type)
  (awg (i minx maxx)
    `(loop for ,i of-type pos-int across ,inds
           minimizing (aref ,a ,i) into ,minx of-type ,type
           maximizing (aref ,a ,i) into ,maxx of-type ,type
           finally (return (values ,minx ,maxx)))))

(defun 2minmax (a n &key type)
  (awg (i minx maxx miny maxy)
    `(loop for ,i of-type pos-int from 0 below (* 2 ,n) by 2
           minimizing (aref ,a ,i) into ,minx of-type ,type
           maximizing (aref ,a ,i) into ,maxx of-type ,type
           minimizing (aref ,a (1+ ,i)) into ,miny of-type ,type
           maximizing (aref ,a (1+ ,i)) into ,maxy of-type ,type
           finally (return (values ,minx ,maxx ,miny ,maxy)))))

(defun 2minmax* (a inds &key type)
  (awg (i ii minx maxx miny maxy)
    `(loop with ,i of-type pos-int
           for ,ii of-type pos-int across ,inds
           do (setf ,i (* 2 ,ii))
           minimizing (aref ,a ,i) into ,minx of-type ,type
           maximizing (aref ,a ,i) into ,maxx of-type ,type
           minimizing (aref ,a (1+ ,i)) into ,miny of-type ,type
           maximizing (aref ,a (1+ ,i)) into ,maxy of-type ,type
           finally (return (values ,minx ,maxx ,miny ,maxy)))))

(defun 3minmax (a n &key type)
  (awg (i minx maxx miny maxy minz maxz)
    `(loop for ,i of-type pos-int from 0 below (* 3 ,n) by 3
           minimizing (aref ,a ,i) into ,minx of-type ,type
           maximizing (aref ,a ,i) into ,maxx of-type ,type
           minimizing (aref ,a (1+ ,i)) into ,miny of-type ,type
           maximizing (aref ,a (1+ ,i)) into ,maxy of-type ,type
           minimizing (aref ,a (+ 2 ,i)) into ,minz of-type ,type
           maximizing (aref ,a (+ 2 ,i)) into ,maxz of-type ,type
           finally (return (values ,minx ,maxx ,miny ,maxy ,minz ,maxz)))))

(defun 3minmax* (a inds &key type)
  (awg (i ii minx maxx miny maxy minz maxz)
    `(loop with i of-type pos-int
           for ,ii of-type pos-int across ,inds
           do (setf ,i (* 3 ,ii))
           minimizing (aref ,a ,i) into ,minx of-type ,type
           maximizing (aref ,a ,i) into ,maxx of-type ,type
           minimizing (aref ,a (1+ ,i)) into ,miny of-type ,type
           maximizing (aref ,a (1+ ,i)) into ,maxy of-type ,type
           minimizing (aref ,a (+ 2 ,i)) into ,minz of-type ,type
           maximizing (aref ,a (+ 2 ,i)) into ,maxz of-type ,type
           finally (return (values ,minx ,maxx ,miny ,maxy ,minz ,maxz)))))

