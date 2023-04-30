(in-package :veq)

(declaim (character *vv-dot* *vv-arr* *vv-bang*)
         (keyword *vv-sym* *vv-nary* *vv-mod* *vv-1ary*
                  *vv-mvc* *vv-red* *vv-map* *vv-xmap*))
(defvar *vv-sym* :!@) (defvar *vv-nary* :_@)
(defvar *vv-mvc* :m@) (defvar *vv-red* :r@)
(defvar *vv-map* :%@) (defvar *vv-xmap* :x@)
(defvar *vv-1ary* :.@) (defvar *vv-mod* :?@)
(defvar *vv-dot* #\.) (defvar *vv-arr* #\$) (defvar *vv-bang* #\!)
(defvar *vv-special* `(,*vv-dot* ,*vv-arr* ,*vv-bang*))

(defun nvrs (a c n) (loop for k from c repeat n collect `(:vr ,a ,k)))
(defun rec-cdr (rec b) `(~ ,@(funcall rec (cdr b))))
(defun gk (p k &optional silent &aux (res (cdr (assoc (kv k) p))))
  (if (or silent res) res (warn "VV: missing conf key: ~a~%conf: ~s" k p)))
(defun gk+ (p &rest keys) (every (lambda (k) (> (gk p k) 0)) keys))
(defun gk0 (p &rest keys) (every (lambda (k) (= (gk p k) 0)) keys))

(defun car-match-modifier (mod b &optional d)
  (declare (optimize speed (safety 2)) (symbol mod))
  "returns (values prefix-ind prefix opt/d b). eg: 1 x (0) arr"
  (labels ((prefix-symb (sym i)
             (when (and i (> i 0)) (psymb (symbol-package sym)
                                          (subseq (mkstr sym) 0 i)))))
    (unless (and (listp b) (symbolp (car b))) (return-from car-match-modifier
                                                (values nil nil d b)))
    (let* ((sym (car b))
           (i (match-substr (mkstr mod) (mkstr sym))))
      (if i (values i (prefix-symb sym i) (or (subseq b 2) d) (second b))
            (values nil nil d b)))))

(defun fx-strip (fx pkg)
  (when fx (psymb pkg (strip-symbols fx *vv-special*))))

; -- ARRAY CONF -----------------------------------------------------------------

(defun vvconf (b &key (vv-sym *vv-sym*))
  (mvb (short-ty fx-full dim dimout) (unpack-veqsymb (car b) :s vv-sym)
    (let* ((pkg (symbol-package (car b)))
           (fx (psymb pkg (nth-value 1 (edge-chars *vv-bang* fx-full t))))
           (fx* (fx-strip fx pkg))
           (ldots (edge-chars *vv-dot* fx)) (rdots (edge-chars *vv-dot* fx t))
           (larrs (edge-chars *vv-arr* fx)) (rarrs (edge-chars *vv-arr* fx t))
           (bangs (edge-chars *vv-bang* fx-full t)))

      (when (and (> bangs 0) (not (= dim dimout))) ; resulting array must be new
        (error "!: inconsistent outdim with !: ~a" b))
      (when (< (length (mkstr fx)) 1) (error "vv: missing fx name in: ~a" b))

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
  (awg (lmod-a lmod-b)
    (labels ((pad-slice (slice &aux (n `(/ (length ,(gk p :lft-sym)) ,(gk p :dim))))
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
  (awg (rmod-a)
    (labels ((pad-slice (slice) ; we never need the second slicer for rht
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
  (mvb (ismod ind) (car-match-modifier *vv-mod* (car b2))
    (values `((:rht . ,(if ismod (cdar b2) b2)) (:@modrht . ,ismod)
              ,@(when (and ismod (> ismod 0)) `((:ind . ,ind)))
              ,@p) b)))

(defun niloutconf (p b) (values `((:out . nil) ,@p) b))

; -- ARRAY HELPERS --------------------------------------------------------------

(defun $row (p i arr)
  (declare (keyword arr i))
  `(-$ ,(gk p :dim) ,(gk p arr) :inds (,(gk p i)) :atype ,(gk p :aty)))

(defun vec-select-itr (p)
  (let ((with-out (if (gk+ p :!) (gk p :lft-sym)
                      `($make :n (* ,(gk p :dimout) ,(gk p :rep-sym))
                              :type ,(gk p :ty) :v ,(type-default (gk p :ty))))))
    `(,@(gk p :@lft t) ,@(gk p :@rht t) ; rep, lft, rht itr via lconf/rconf
     ,@(when (gk p :out t) ; output array
         `(with ,(gk p :out-sym) of-type ,(gk p :aty) = ,with-out))
      repeat ,(gk p :rep-sym) ; repeat, via lconf
      for ,(gk p :ind) of-type pn = ,(gk p :itr-lft-sym) ; local ind
      ,@(when (gk p :out t) ; out array index
          `(for ,(gk p :itr-out-sym) of-type pn
             ,@(if (gk+ p :!) `(= ,(gk p :itr-lft-sym)) `(from 0)))))))

