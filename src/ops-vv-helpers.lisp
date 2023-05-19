(in-package :veq)

(declaim (character *vv-dot* *vv-arr* *vv-bang*)
         (keyword *vv-!@* *vv-_@* *vv-?@* *vv-.@*
                  *vv-m@* *vv-r@* *vv-%@* *vv-x@*))

(defvar *vv-!@* :!@) (defvar *vv-_@* :_@)
(defvar *vv-m@* :m@) (defvar *vv-r@* :r@)
(defvar *vv-%@* :%@) (defvar *vv-x@* :x@)
(defvar *vv-.@* :.@) (defvar *vv-?@* :?@)
(defvar *vv-dot* #\.) (defvar *vv-arr* #\$) (defvar *vv-bang* #\!)

(defvar *vv-special*
        (mapcar #'mkstr `(,*vv-dot* ,*vv-arr* ,*vv-bang*)))

(defun nvrs (a c n) (loop for k from c repeat n collect `(:vr ,a ,k)))

(defun gk (p k &optional silent &aux (hit (cdr (assoc k p))))
  (declare (optimize speed (safety 2)) (list p) (keyword k))
  (if (or silent hit) hit (warn "VV: missing conf key: ~a~%conf: ~s" k p)))
(defun gk+ (p &rest keys)
  ; (declare (optimize speed (safety 2)) (list p keys))
  (every (lambda (k) (> (gk p k) 0)) keys))
(defun gk0 (p &rest keys)
  ; (declare (optimize speed (safety 2)) (list p keys))
  (every (lambda (k) (= (gk p k) 0)) keys))

(defun vverr (expr msg)
  (error "~&VV error at~%sym: ~a~%msg: ~a~%expr:~s~&" (car expr) msg expr))

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
                    (when (> (length b) 2) (subseq b 2))
                    (second b))
          (values nil nil nil b))))

; -- ARRAY CONF -----------------------------------------------------------------

(defun niloutconf (p b) (values `((:out . nil) ,@p) b))

(defun tailconf (p b &aux (b2 (subseq b 2)))
  (declare (optimize speed) (list p b))
  (mvb (ismod ind) (car-match-modifier *vv-?@* (car b2))

    (values `((:rht . ,(if ismod (cdar b2) b2)) (:@modrht . ,ismod)
              ,@(when (and ismod (> (the pn ismod) 0)) `((:ind . ,ind)))
              ,@p) b)))

(defun fx-strip (fx pkg) (when fx (psymb pkg (strip-symbols fx *vv-special*))))

(defun vvconf (b vv-sym &aux (s (car b)))
  (declare (optimize speed (safety 2)) (list b) (symbol s))
  (mvb (short-ty sfx-full dim dimout) (unpack-vvsym s :s vv-sym :symout nil)
    (declare (pn dim dimout))
    (let* ((pkg (symbol-package s))
           (sfx (nth-value 1 (edge-str *vv-bang* sfx-full t)))
           (fx (psymb pkg sfx)) (fx* (fx-strip sfx pkg))
           (ldots (edge-str *vv-dot* sfx)) (rdots (edge-str *vv-dot* sfx t))
           (larrs (edge-str *vv-arr* sfx)) (rarrs (edge-str *vv-arr* sfx t))
           (bangs (edge-str *vv-bang* sfx-full t)))
      (declare (symbol fx) (string sfx) (pn ldots rdots larrs rarrs bangs))
      (when (and (> bangs 0) (not (= dim dimout))) (vverr b "bad outdim"))
      (when (< (length sfx) 1) (vverr b "missing fx name"))
      (when (> (* larrs rarrs) 1) (vverr b "too many arrays ($)"))
      (when (> (* ldots rdots) 0) (vverr b "broadcasting (.) on both sides"))

      `((:dim . ,dim) (:dimout . ,dimout) (:pkg . ,pkg) (:fx . ,fx) (:fx* . ,fx*)
        (:ty . ,(type-from-short short-ty t)) (:aty . ,(arrtype short-ty 'vector))
        (:.l . ,ldots) (:.r . ,rdots) (:dots . ,(max ldots rdots))
        (:$l . ,larrs) (:$r . ,rarrs) (:! . ,bangs)
        ; defaults:
        (:ind . ind) (:out . t) (:lft-sym . ,(gensym "LFT")) (:rht-sym . ,(gensym "RHT"))
        (:itr-lft-sym . ,(gensym "ITR-LFT")) (:itr-rht-sym . ,(gensym "ITR-RHT"))
        (:itr-out-sym . ,(gensym "ITR-OUT")) (:out-sym . ,(gensym "OUT"))
        (:rep-sym . ,(gensym "REP"))
        (:mode . ,vv-sym) (:sym . ,s)))))

(defun ?@-index-type (o) (ecase o (:l 'list) (:p 'pvec) (:i 'ivec) (:v 'vector)))
(defun ?@-loop-type (o) (ecase o (:l 'in) ((:p :i :v) 'across)))

; NOTE: rep is controlled by the lft iterator
(defun lconf (p b &aux (dim (gk p :dim)) (ty (gk p :ty t)))
  (declare (optimize speed) (list p b) (pn dim))
  (awg (lmod-a lmod-b)
    (labels ((pad-slice (slice &aux (n `(/ (length ,(gk p :lft-sym)) ,dim)))
               (declare (list slice) )
               (case (length slice)
                 (0 `(0 ,n)) (1 `(,(car slice) ,n)) (2 slice)
                 (otherwise (vverr b "?@ (lft): bad # of elements")))))
      (mvb (ismod opt slice expr) (car-match-modifier *vv-?@* (second b))
        (declare (ignore ismod))
        (unless expr (vverr b "?@ (lft): missing arg"))
        (let ((opt (kv opt)))
          (values
           (case opt
             (:z `((:lft with ,lmod-a of-type pn = ,expr
                         with ,(gk p :lft-sym) of-type ,(gk p :aty) =
                                   ($make :n (* ,(gk p :dimout) ,lmod-a)
                                          :type ,ty :v ,(type-default ty 0))
                         with ,(gk p :rep-sym) of-type pn = ,lmod-a
                         for ,(gk p :itr-lft-sym) of-type pn from 0)
                   (:! . 1) ,@p))
             ((:l :p :i :v)
                `((:lft with ,(gk p :lft-sym) of-type ,(gk p :aty) = ,expr
                        with ,lmod-a of-type ,(?@-index-type opt) = ,(first slice)
                        with ,(gk p :rep-sym) of-type pn = (length ,lmod-a)
                        for ,(gk p :itr-lft-sym) of-type pn ,(?@-loop-type opt) ,lmod-a)
                  ,@p))
             (:nil (let ((slice (pad-slice slice))) ; default for (?@ is slicing)
                     `((:lft with ,(gk p :lft-sym) of-type ,(gk p :aty) = ,expr
                             with ,lmod-a of-type pn = ,(first slice) and
                             ,lmod-b of-type pn = ,(second slice)
                             with ,(gk p :rep-sym) of-type pn = (- ,lmod-b ,lmod-a)
                             for ,(gk p :itr-lft-sym) of-type pn from ,lmod-a)
                       ,@p)))
             (otherwise (vverr b "?@ (lft): unexpected option")))
           b))))))

(defun rconf (p b)
  (declare (optimize speed) (list p b))
  (awg (rmod-a)
    (labels ((pad-slice (slice) ; we never need the second slicer for rht
               (declare (list slice))
               (case (length slice) (0 `(0)) ((1 2) `(,(first slice)))
                     (otherwise (vverr b "?@ (rht): bad # of elements")))))

      (mvb (ismod opt slice expr) (car-match-modifier *vv-?@* (third b))
        (declare (ignore ismod))
        (unless expr (vverr b "?@ (rht): missing arg"))
        (let ((opt (kv opt)))
          (values
           (case opt
             ((:l :p :i :v)
              `((:rht . ,expr)
                (:rht-mod with ,rmod-a of-type ,(?@-index-type opt) = ,(first slice)
                          for ,(gk p :itr-rht-sym) of-type pn ,(?@-loop-type opt) ,rmod-a)
                ,@p))
             (:nil (let ((slice (pad-slice slice)))
                     `((:rht . ,expr)
                       (:rht-mod with ,rmod-a of-type pn = ,(first slice)
                                 for ,(gk p :itr-rht-sym) of-type pn from ,rmod-a)
                       ,@p)))
             (otherwise (vverr b "?@ (rht): unexpected option")))
           b))))))

; -- ARRAY HELPERS --------------------------------------------------------------

(defun $row (p i arr)
  (declare (keyword arr i))
  `(-$ ,(gk p :dim) ,(gk p arr) :inds (,(gk p i)) :atype ,(gk p :aty)))

(defun vec-select-itr (p)
  (declare (optimize speed))
  (let ((with-out (if (gk+ p :!) (gk p :lft-sym)
                    `($make :n (* ,(gk p :dimout) ,(gk p :rep-sym))
                            :type ,(gk p :ty) :v ,(type-default (gk p :ty) nil)))))
    `(,@(gk p :lft t) ,@(gk p :rht-mod t) ; from lconf/rconf
      ,@(when (gk p :out t) ; output array
              `(with ,(gk p :out-sym) of-type ,(gk p :aty) = ,with-out))
      repeat ,(gk p :rep-sym) ; repeat, via lconf
      for ,(gk p :ind) of-type pn = ,(gk p :itr-lft-sym) ; local ind
      ,@(when (gk p :out t) ; out array index
          `(for ,(gk p :itr-out-sym) of-type pn
             ,@(if (gk+ p :!) `(= ,(gk p :itr-lft-sym)) `(from 0)))))))

