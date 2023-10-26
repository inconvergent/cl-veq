(in-package :veq)

(declaim (character *vv-dot* *vv-arr* *vv-bang*)
         (keyword *vv-!@* *vv-_@* *vv-?@* *vv-.@*
                  *vv-m@* *vv-f@*
                  *vv-r@* *vv-%@* *vv-x@*))

(defvar *vv-!@* :!@) (defvar *vv-_@* :_@)
(defvar *vv-m@* :m@) (defvar *vv-f@* :f@) (defvar *vv-a@* :a@)

(defvar *vv-r@* :r@)
(defvar *vv-%@* :%@) (defvar *vv-x@* :x@)
(defvar *vv-.@* :.@) (defvar *vv-?@* :?@)
(defvar *vv-dot* #\.) (defvar *vv-bang* #\!)
(defvar *vv-arr* #\$) (defvar *vv-simd* #\&)

(defvar *vv-special* (mapcar #'mkstr `(,*vv-dot*  ,*vv-bang* ,*vv-arr* ,*vv-simd*)))

(defvar *vverr-msg* "~&██ VV error at: ~a~%██ msg: ~a~%██ xpr: ~s~&")
(defun vverr (expr msg) (error *vverr-msg* (car expr) msg expr))
(defmacro vverr-len (b wanted got)
  `(unless ,wanted
     (vverr ,b (format nil "bad # of elements. wanted ~a, got: ~a" ',wanted ,got))))


(defun nvrs (a c n) (loop for k from c repeat n collect `(:vr ,a ,k)))

(defun gk (p k &optional silent &aux (hit (cdr (assoc k p))))
  (declare #.*opt* (list p) (keyword k))
  (if (or silent hit) hit (warn "VV: missing conf key: ~a~%conf: ~s" k p)))
(defun gk+ (p &rest keys)
  (declare #.*opt* (list p keys))
  (every (lambda (k) (> (gk p k) 0)) keys))
(defun gk0 (p &rest keys)
  (declare #.*opt* (list p keys))
  (every (lambda (k) (= (gk p k) 0)) keys))


(defun car-match-modifier (mod b)
  (declare #.*opt* (symbol mod))
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

; NOTE/ TODO: "$"/"" -> ||
(defun ensure-fx-sym (p &aux (fx (gk p :fx*)))
  "return gensym :fx* is the empty string. otherwise the symbol will be ||"
  (if (< (length (mkstr fx)) 1) (gensym "VV-NONAME") fx))
(defun assert-fx-sym (b p &aux (fx (gk p :fx*)))
  "error when :fx* is empty"
  (if (< (length (mkstr fx)) 1) (vverr b "missing fx name") fx))
(defun strip-fx-psymb (sfx pkg)
  (declare (string sfx))
  (psymb pkg (strip-symbols sfx *vv-special*)))

; -- ARRAY CONF -----------------------------------------------------------------

(defun niloutconf (p b) (values `((:out . nil) ,@p) b))

(defun tailconf (p b &aux (b2 (subseq b 2)))
  (declare #.*opt* (list p b))
  (mvb (ismod ind) (car-match-modifier *vv-?@* (car b2))
    (values `((:rht . ,(if ismod (cdar b2) b2)) (:@modrht . ,ismod)
              ,@(when (and ismod (> (the pn ismod) 0)) `((:ind . ,ind)))
              ,@p) b)))

(defun vvconf (b vv-sym &aux (s (car b)))
  (declare #.*opt* (list b) (symbol s))
  (mvb (short-ty sfx-full dim dimout) (unpack-vvsym s :s vv-sym :symout nil)
    (declare (pn dim dimout))
    (let* ((pkg (symbol-package s))
           (sfx (nth-value 1 (edge-str *vv-bang* sfx-full t)))
           (fx (psymb pkg sfx)) (fx* (strip-fx-psymb sfx pkg))
           (ldots (edge-str *vv-dot* sfx)) (rdots (edge-str *vv-dot* sfx t))
           (larrs (edge-str *vv-arr* sfx)) (rarrs (edge-str *vv-arr* sfx t))
           (lsimd (edge-str *vv-simd* sfx)) (rsimd (edge-str *vv-simd* sfx t))
           (bangs (edge-str *vv-bang* sfx-full t)))
      (declare (symbol fx) (string sfx)
               (pn ldots rdots larrs rarrs bangs lsimd rsimd))
      (when (and (> bangs 0) (not (= dim dimout))) (vverr b "bad outdim"))
      (when (> (+ larrs rarrs) 2) (vverr b "too many arrays ($)"))
      (when (> (* ldots rdots) 0) (vverr b "broadcasting (.) on both sides"))
      ; TODO: SIMD is not implemented for any cases!!
      (when (or (> (* larrs rsimd) 0) (> (* lsimd rarrs) 0))
            (vverr b "must use either $ or & (not implemented) for broadcasts, not both"))

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
  (declare #.*opt* (list p b) (pn dim))
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
  (declare #.*opt* (list p b))
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

(defun $mvb-row (p side body &aux (ty (gk p :ty)) (dim (gk p :dim)))
  (declare #.*opt*)
  (let* ((here (gensym "IND"))
         (vars (loop for i from 0 repeat dim
                    collect `(,(gensym) (aref ,(gk p (keyw side :-sym))
                                              (the veq:pn (+ ,here ,i))))))
         (names (mapcar #'car vars)))
    `(let* ((,here (* ,dim ,(gk p (keyw :itr- side :-sym)))) ,@vars)
       (declare (veq:pn ,here) (ignorable ,@names)
                ,@(unless (or (null ty) (equal :nil ty)) `((,ty ,@names))))
       ; only replace references to side in row/body
       ,@(replace-varg body (list (cons side names)) t))))

; NOTE: this can only be used in !@$fx$, unless modified
(defun $$mvb-row (p row &aux (dim (gk p :dim)) (dimout (gk p :dimout)))
  (declare #.*opt*)
  (awg (ind-lft ind-rht ind-out)
    (labels (($ (a i j) `(aref ,(gk p a) (the veq:pn (+ ,i ,j)))))
      (let ((lft-arefs (loop for i from 0 repeat dimout
                             collect ($ :lft-sym ind-lft i)))
            (rht-arefs (loop for i from 0 repeat dimout
                             collect ($ :rht-sym ind-rht i)))
            (full-row  (loop for i from 0 for r in row repeat dimout
                             nconc `(,($ :out-sym ind-out i) ,r))))
       ; TODO: only use one index when possible.
       `(let ((,ind-lft (* ,dim ,(gk p :itr-lft-sym)))
              (,ind-rht (* ,dim ,(gk p :itr-rht-sym)))
              (,ind-out (* ,dimout ,(gk p :itr-out-sym))))
          (declare (veq:pn ,ind-lft ,ind-rht ,ind-out))
          ,(replace-varg `(setf ,@full-row)
                         `((lft ,@lft-arefs) (rht ,@rht-arefs)) t))))))

(defun vec-select-itr (p)
  (declare #.*opt*)
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

