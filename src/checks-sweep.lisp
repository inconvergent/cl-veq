(in-package :veq)

(deftype line-vec () `(vector fvec))
(deftype segx-vec () `(simple-array list))

(defun segx-vec (n) (declare #.*opt* (pn n))
  (make-array n :element-type 'list :initial-element nil :adjustable nil))

; TODO: special cases: equal x pos, vertical line
; TODO: some related notes on improvements:
; TODO: we dont need to do full segx test after sweep line match?
; TODO: binary search tree to store current state. or hash table?
; https://en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm
(fvdef f2lsegx (lines)
  (declare #.*opt* (line-vec lines)) "find all line-line intersections in lines"
  (labels ((sort-pairs (lines)
             (loop with res = (list)
               for l of-type fvec across lines for i of-type pn from 0
               do (push `(,(aref l 0) . ,i) res) ; ((x00 l0) (x01 l0) ..)
                  (push `(,(aref l 2) . ,i) res)
               finally (return (sort res #'< :key #'car)))))
    (let* ((pts (sort-pairs lines))
           (state (list (cdar pts)))
           (res (segx-vec (length lines))))
      (declare (list pts state) (segx-vec res))
      (labels ((add-state (i k p) (declare (pn i k) (ff p))
                 (if (aref res i) (push `(,k . ,p) (aref res i))
                                  (setf (aref res i) `((,k . ,p)))))
               (del-state (i) (declare(pn i))
                 (setf state (remove-if #'(lambda (e) (declare (pn e)) (eql e i))
                                        state)))
               (isect-state (i cands) (declare (pn i) (list cands))
                 (loop with l of-type fvec = (aref lines i)
                   for k of-type pn in cands
                   do (mvb (isect p q) (f2segx (f2$ l 0 1) (f2$ (aref lines k) 0 1))
                           (declare (boolean isect) (ff p q))
                           (when isect (add-state i k p)
                                       (add-state k i q))))))
        (loop for o of-type cons in (cdr pts) for i of-type pn = (cdr o)
              if (member i state) do (del-state i)
              else do (isect-state i state)
                      (push i state)))
      res)))

; NOTE: this is very similar to f2lsegx. keep it separated for now
(fvdef f2ssegx (lines k &optional (n (length lines)))
  (declare #.*opt* (line-vec lines) (pn k n))
  "find all line-line intersections between the first k lines, with the remaining n-k lines"
  (labels ((sort-pairs (lines)
             (loop with res = (list)
                   for l of-type fvec across lines for i of-type pn from 0 below n
                   do (push `(,(aref l 0) . ,i) res) ; ((x00 l0) (x01 l0) ..)
                      (push `(,(aref l 2) . ,i) res)
                   finally (return (sort res #'< :key #'car)  ))))
    (let* ((res (segx-vec n))
           (pts (sort-pairs lines)) (state (list (cdar pts))))
      (declare (list pts state) (segx-vec res))
      (labels ((add-state (i k p) (declare (pn i k) (ff p))
                 (if (aref res i) (push `(,k . ,p) (aref res i))
                                  (setf (aref res i) `((,k . ,p)))))
               (del-state (i) (declare (pn i))
                 (setf state (remove-if #'(lambda (v) (declare (pn v)) (eql v i)) state)))
               (isect-state (i cands &aux (iside (< i k)))
                 (declare (pn i) (list cands) (boolean iside))
                 (loop with l of-type fvec = (aref lines i)
                       for v of-type pn in cands
                       if (not (equal (< v k) iside))
                       do (mvb (isect sp sq) (f2segx (f2$ l 0 1) (f2$ (aref lines v) 0 1))
                               (declare (boolean isect) (ff sp sq))
                               (when isect (add-state i v sp)
                                           (add-state v i sq))))))
        (loop for o of-type cons in (cdr pts) for i of-type pn = (cdr o)
              if (member i state) do (del-state i)
              else do (isect-state i state)
                      (push i state)))
      res)))
