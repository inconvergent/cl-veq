(in-package :veq)

(declaim (character *vv-dot* *vv-arr* *vv-bang*)
         (keyword *vv-sym* *vv-nary* *vv-mod* *vv-1ary*
                  *vv-mvc* *vv-red* *vv-map* *vv-xmap*))

(defvar *vv-sym* :!@) (defvar *vv-nary* :_@)
(defvar *vv-mvc* :m@) (defvar *vv-red* :r@)
(defvar *vv-map* :%@) (defvar *vv-xmap* :x@)
(defvar *vv-1ary* :.@) (defvar *vv-mod* :?@)
(defvar *vv-dot* #\.) (defvar *vv-arr* #\$) (defvar *vv-bang* #\!)

(defvar *vv-special*
        (mapcar #'mkstr `(,*vv-dot* ,*vv-arr* ,*vv-bang*)))

(defun nvrs (a c n) (loop for k from c repeat n collect `(:vr ,a ,k)))
(defun rec-cdr (rec b) `(~ ,@(funcall rec (cdr b))))

(defun gk (p k &optional silent &aux (hit (cdr (assoc k p))))
  (declare (optimize speed) (list p) (keyword k))
  (if (or silent hit) hit (warn "VV: missing conf key: ~a~%conf: ~s" k p)))

(defun gk+ (p &rest keys) (every (lambda (k) (> (gk p k) 0)) keys))
(defun gk0 (p &rest keys) (every (lambda (k) (= (gk p k) 0)) keys))


(declaim (inline car-match-trigger))
(defun car-match-trigger (trig b)
  (declare (optimize speed (safety 2)) (string trig))
  "returns (values prefix-ind prefix opt/d b). eg: 1 x (0) arr"
  (unless (and (listp b) (symbolp (car b)))
          (return-from car-match-trigger nil))
  (match-substr trig (mkstr (car b))))

(defun car-match-modifier (mod b)
  (declare (optimize speed (safety 2)) (symbol mod))
  "returns (values prefix-ind prefix opt/d b). eg: 1 x (0) arr"
  (unless (and (listp b) (symbolp (car b)))
          (return-from car-match-modifier (values nil nil nil b)))
  (let* ((sym (car b))
         (str (mkstr sym))
         (i (match-substr (mkstr mod) str)))
    (declare (symbol sym) (string str))
    (if i (values i (when (> (the pn i) 0)
                          (psymb (symbol-package sym) (subseq str 0 i)))
                    (subseq b 2)
                    (second b))
          (values nil nil nil b))))


(defun fx-strip (fx pkg)
  (when fx (psymb pkg (strip-symbols fx *vv-special*))))

; -- ARRAY CONF -----------------------------------------------------------------

(defun vvconf (b &key (vv-sym *vv-sym*))
  (declare (optimize speed) (list b))
  (mvb (short-ty fx-full-str dim dimout) (unpack-vvsym (car b)
                                           :s vv-sym :symout nil)
    (declare (pn dim dimout))
    (let* ((pkg (symbol-package (car b)))
           (fx-str (nth-value 1 (edge-chars-str *vv-bang* fx-full-str t)))
           (fx (psymb pkg fx-str))
           (fx* (fx-strip fx-str pkg))
           (ldots (edge-chars-str *vv-dot* fx-str))
           (rdots (edge-chars-str *vv-dot* fx-str t))
           (larrs (edge-chars-str *vv-arr* fx-str))
           (rarrs (edge-chars-str *vv-arr* fx-str t))
           (bangs (edge-chars-str *vv-bang* fx-full-str t)))
      (declare (symbol fx) (string fx-str)
               (pn ldots rdots larrs rarrs bangs))
      (when (and (> bangs 0) (not (= dim dimout))) ; resulting array must be new
        (error "!: inconsistent outdim with !: ~a" b))
      (when (< (length fx-str) 1) (error "vv: missing fx name in: ~a" b))

      `((:dim . ,dim) (:dimout . ,dimout) (:pkg . ,pkg) (:! . ,bangs)
        (:fx . ,fx) (:fx* . ,fx*)
        (:ty . ,(type-from-short short-ty t))
        (:aty . ,(arrtype short-ty 'vector))
        (:$l . ,larrs) (:.l . ,ldots) (:$r . ,rarrs) (:.r . ,rdots)
        (:dots . ,(max ldots rdots)) (:arrs . ,(max larrs rarrs))
        ; defaults:
        (:@lft) (:@rht) (:ind . ind) (:out . t)
        (:lft-sym . ,(gensym "LFT")) (:rht-sym . ,(gensym "RHT"))
        (:itr-lft-sym . ,(gensym "ITR-LFT")) (:itr-rht-sym . ,(gensym "ITR-RHT"))
        (:itr-out-sym . ,(gensym "ITR-OUT")) (:out-sym . ,(gensym "OUT"))
        (:rep-sym . ,(gensym "REP"))))))

(defun ?@-index-type (o) (ecase o (:l 'list) (:p 'pvec) (:i 'ivec) (:v 'vector)))
(defun ?@-loop-type (o) (ecase o (:l 'in) ((:p :i :v) 'across)))

; NOTE: rep is controlled by the lft iterator
(defun lconf (p b)
  (declare (optimize speed) (list p b))
  (awg (lmod-a lmod-b)
    (labels ((pad-slice (slice &aux (n `(/ (length ,(gk p :lft-sym)) ,(gk p :dim))))
               (declare (list slice) )
               (case (length slice)
                 (0 `(0 ,n)) (1 `(,(car slice) ,n)) (2 slice)
                 (otherwise (error "?@: lconf bad # of elements: ~a" b)))))

      (mvb (ismod opt slice expr) (car-match-modifier *vv-mod* (second b))
        (declare (ignore ismod))
        (let ((opt (kv opt)))
         (values
           (case opt
             ((:l :p :i :v)
                `((:lft . ,expr)
                  (:@lft with ,lmod-a of-type ,(?@-index-type opt) = ,(first slice)
                         with ,(gk p :rep-sym) of-type pn = (length ,lmod-a)
                         for ,(gk p :itr-lft-sym) of-type pn ,(?@-loop-type opt) ,lmod-a)
                  ,@p))
             (:nil (let ((slice (pad-slice slice)))
                     `((:lft . ,expr)
                       (:@lft with ,lmod-a of-type pn = ,(first slice) and
                                   ,lmod-b of-type pn = ,(second slice)
                              with ,(gk p :rep-sym) of-type pn = (- ,lmod-b ,lmod-a)
                              for ,(gk p :itr-lft-sym) of-type pn from ,lmod-a)
                       ,@p)))
             (otherwise (error "?@: bad lconf for: ~a" b)))
           b))))))

(defun rconf (p b)
  (declare (optimize speed) (list p b))
  (awg (rmod-a)
    (labels ((pad-slice (slice) ; we never need the second slicer for rht
               (declare (list slice) )
               (case (length slice) (0 `(0)) ((1 2) `(,(first slice)))
                     (otherwise (error "?@: rconf bad # of elements: ~a" b)))))

      (mvb (ismod opt slice expr) (car-match-modifier *vv-mod* (third b))
        (declare (ignore ismod))
        (let ((opt (kv opt)))
          (values
           (case (kv opt)
             ((:l :p :i :v)
              `((:rht . ,expr)
                (:@rht with ,rmod-a of-type ,(?@-index-type opt) = ,(first slice)
                       for ,(gk p :itr-rht-sym) of-type pn ,(?@-loop-type opt) ,rmod-a)
                ,@p))
             (:nil (let ((slice (pad-slice slice)))
                     `((:rht . ,expr)
                       (:@rht with ,rmod-a of-type pn = ,(first slice)
                              for ,(gk p :itr-rht-sym) of-type pn from ,rmod-a)
                       ,@p)))
             (otherwise (error "VV: bad rconf: ~a" b)))
           b))))))

(defun tailconf (p b &aux (b2 (subseq b 2)))
  (declare (optimize speed) (list p b))
  (mvb (ismod ind) (car-match-modifier *vv-mod* (car b2))

    (values `((:rht . ,(if ismod (cdar b2) b2)) (:@modrht . ,ismod)
              ,@(when (and ismod (> (the pn ismod) 0)) `((:ind . ,ind)))
              ,@p) b)))

(defun niloutconf (p b) (values `((:out . nil) ,@p) b))

; -- ARRAY HELPERS --------------------------------------------------------------

(defun $row (p i arr)
  (declare (keyword arr i))
  `(-$ ,(gk p :dim) ,(gk p arr) :inds (,(gk p i)) :atype ,(gk p :aty)))

(defun vec-select-itr (p)
  (declare (optimize speed))
  (let ((with-out (if (gk+ p :!) (gk p :lft-sym)
                      `($make :n (* ,(gk p :dimout) ,(gk p :rep-sym))
                              :type ,(gk p :ty) :v ,(type-default (gk p :ty) nil)))))
    `(,@(gk p :@lft t) ,@(gk p :@rht t) ; rep, lft, rht itr via lconf/rconf
     ,@(when (gk p :out t) ; output array
         `(with ,(gk p :out-sym) of-type ,(gk p :aty) = ,with-out))
      repeat ,(gk p :rep-sym) ; repeat, via lconf
      for ,(gk p :ind) of-type pn = ,(gk p :itr-lft-sym) ; local ind
      ,@(when (gk p :out t) ; out array index
          `(for ,(gk p :itr-out-sym) of-type pn
             ,@(if (gk+ p :!) `(= ,(gk p :itr-lft-sym)) `(from 0)))))))

