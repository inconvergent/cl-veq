(in-package :veq)

(declaim (character *vv-dot* *vv-arr* *vv-bang*)
         (keyword *vv-sym* *vv-nary* *vv-ind* *vv-1ary* *vv-mvc*))

(defvar *vv-sym* :!@) (defvar *vv-nary* :_@) (defvar *vv-mvc* :m@)
(defvar *vv-1ary* :.@) (defvar *vv-ind* :@)
(defvar *vv-dot* #\.) (defvar *vv-arr* #\$) (defvar *vv-bang* #\!)
(defvar *vv-special* `(,*vv-dot* ,*vv-arr* ,*vv-bang*))

(defun nvrs (a c n) (loop for k from c repeat n collect `(:vr ,a ,k)))
(defun rec-cdr (rec b) `(~ ,@(funcall rec (cdr b))))
(defun gk (p k &aux (res (cdr (assoc (kv k) p))))
  (if res res (warn "VV: missing conf key: ~a~%conf: ~s" k p)))
(defun gk+ (p &rest keys) (every (lambda (k) (> (gk p k) 0)) keys))
(defun gk0 (p &rest keys) (every (lambda (k) (= (gk p k) 0)) keys))

(defun @? (b) (and (listp b) (eq (kv (car b)) *vv-ind*))) ; is this a slicer?
(defun @strip (b) (cond ((@? b) (second b)) (t b))) ; remove @ wrapper
; TODO: document slicing
; TODO: fix leaky slicing bindings
(defun @validate (b)
  (unless (every #'atom b) (error "slicers must be atoms, got: ~a" b))
  b)
(defun @slicer (b) (if (@? b) (@validate (subseq b 2)) '(0))) ; get slicer or (0)

; -- MVC ------------------------------------------------------------------------

(defun dom@fx (rec b p) `(mvc #',(gk p :fx*) ,(rec-cdr rec b)))

; -- NARY -----------------------------------------------------------------------

(defun do.@fx (rec b p &aux (dim (gk p :dim))) ; elem wise. eg for abs
  (-vmvb* (gk p :ty) dim 'arg (rec-cdr rec b)
    `((values ,@(loop for d from 0 repeat dim
            collect `(,(gk p :fx*) (:vr arg ,d)))))))

(defun do_@fx (rec b p &aux (dim (gk p :dim))) ; row wise. eg cross
  (-vmvb* (gk p :ty) dim 'arg (rec-cdr rec b)
    `((,(gk p :fx*) ,@(loop for d from 0 repeat dim
                            collect `(:vr arg ,d))))))

; -- VEC ---------------------------------------------------------------------

(defun do!@fx (rec b p &aux (dim (gk p :dim))) ; row wise, pairs eg +
  (-vmvb* (gk p :ty) (* 2 dim) 'arg (rec-cdr rec b)
    `((values
        ,@(loop for d from 0 repeat dim
                collect `(,(gk p :fx*) (:vr arg ,d) (:vr arg ,(+ d dim))))))))

(defun do!@.fx (rec b p &aux (dim (gk p :dim)))
  (-vmvb* (gk p :ty) (+ dim (gk p :dots)) 'arg (rec-cdr rec b)
    `((values
        ,@(loop with lhs = (nvrs 'arg 0 (gk p :dots))
                for d from 0 repeat dim
                collect `(,(gk p :fx*) ,@lhs (:vr arg ,(+ (gk p :dots) d))))))))

(defun do!@fx. (rec b p &aux (dim (gk p :dim)) (dots (gk p :dots)))
  (-vmvb* (gk p :ty) (+ dim dots) 'arg (rec-cdr rec b)
    `((values ,@(loop with rhs = (nvrs 'arg dim dots)
                      for d from 0 repeat dim
                      collect `(,(gk p :fx*) (:vr arg ,d) ,@rhs))))))

; -- BCAST HELPERS --------------------------------------------------------------

; TODO: make adjustable dimout
(defun vvconf (b &key (vv-sym *vv-sym*))
  (mvb (short-ty dim fx-full) (unpack-veqsymb (car b) :s vv-sym)
    (let* ((pkg (symbol-package (car b)))
           (fx (psymb pkg (nth-value 1 (edge-chars *vv-bang* fx-full t))))
           (ldots (edge-chars *vv-dot* fx)) (rdots (edge-chars *vv-dot* fx t))
           (larrs (edge-chars *vv-arr* fx)) (rarrs (edge-chars *vv-arr* fx t)))
      `((:dim . ,dim) (:dimout . ,dim) (:pkg . ,pkg)
        (:fx . ,fx) (:fx* . ,(psymb pkg (strip-chars fx *vv-special*)))
        (:ty . ,(type-from-short short-ty t))
        (:aty . ,(arrtype short-ty 'vector))
        (:$l . ,larrs) (:.l . ,ldots) (:$r . ,rarrs) (:.r . ,rdots)
        (:! . ,(edge-chars *vv-bang* fx-full t))
        (:dots . ,(max ldots rdots)) (:arrs . ,(max larrs rarrs))))))

(defun symconf (p b)
  (values `((:lft-sym . ,(gensym "LFT")) (:rht-sym . ,(gensym "RHT"))
            (:itr-lft-sym . ,(gensym "ITR-LFT"))
            (:itr-rht-sym . ,(gensym "ITR-RHT"))
            (:itr-out-sym . ,(gensym "ITR-OUT"))
            (:out-sym . ,(gensym "OUT")) (:rep-sym . ,(gensym "REP"))
            ,@p) b))

(defun sliceconf (p b &aux (rng (gk p :@lft))
                           (n `(/ (length ,(gk p :lft-sym)) ,(gk p :dim))))
  (values `((:rep . ,(cond ((= 2 (length rng)) `(- ,(second rng) ,(first rng)))
                           ((= 1 (length rng)) `(- ,n ,(first rng)))
                           (t n))) ,@p) b))

(defun $row (p i arr)
  (declare (keyword arr i))
  `(-$ ,(gk p :dim) ,(gk p arr) :inds (,(gk p i)) :atype ,(gk p :aty)))

(defun lconf (p b &aux (v (second b)))
  (values `((:lft . ,(@strip v)) (:@lft . ,(@slicer v)) ,@p) b))
(defun rconf (p b &aux (v (third b)))
  (values `((:rht . ,(@strip v)) (:@rht . ,(@slicer v)) ,@p) b))

(defun vec-select-itr (p &optional r)
  `(with ,(gk p :rep-sym) of-type pn = ,(gk p :rep)
    with ,(gk p :out-sym) of-type ,(gk p :aty) =
         ,(if (gk+ p :!) (gk p :lft-sym)
            `($make :n (* ,(gk p :dim) ,(gk p :rep-sym))
                    :type ,(gk p :ty) :v ,(type-default (gk p :ty))))
    for ,(gk p :itr-lft-sym) of-type pn from ,(nth* (gk p :@lft) 0 0)
    repeat ,(gk p :rep-sym)
    ,@(when r `(for ,(gk p :itr-rht-sym) of-type pn from ,(nth* (gk p :@rht) 0 0)))
    for ,(gk p :itr-out-sym) of-type pn
        ,@(if (gk+ p :!) `(= ,(gk p :itr-lft-sym)) `(from 0))))

; -- ARRAY LEFT NARY ------------------------------------------------------------

(defun do.@$fx (rec b p &aux (dim (gk p :dim)) (ty (gk p :ty)) (aty (gk p :aty)))
  (let ((p (vchain (#'sliceconf #'lconf #'symconf) p b))
        (row `(values ,@(loop for d from 0 repeat dim
                              collect `(,(gk p :fx*) (:vr lft ,d))))))
    `(loop with ,(gk p :lft-sym) of-type ,aty = ,(funcall rec (gk p :lft))
           ,@(vec-select-itr p)
           do ,(-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                 `(($nvset (,(gk p :out-sym) ,dim (* ,(gk p :itr-out-sym) ,dim))
                           ,row)))
           finally (return ,(gk p :out-sym)))))

(defun do_@$fx. (rec b p &aux (dim (gk p :dim)) (ty (gk p :ty)) (aty (gk p :aty)))
  (let* ((p (vchain (#'sliceconf #'lconf #'symconf) p b))
    (rhs (nvrs 'rht 0 (gk p :dots)))
        (row `(,(gk p :fx* )
               ,@(loop for d from 0 repeat dim collect `(:vr lft ,d))
               ,@rhs)))
    (-vmvb* ty (gk p :dots) 'rht `(~ ,@(funcall rec (subseq b 2)))
     `((loop with ,(gk p :lft-sym) of-type ,aty = ,(funcall rec (gk p :lft))
           ,@(vec-select-itr p)
           do ,(-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                 `(($nvset (,(gk p :out-sym) ,dim (* ,(gk p :itr-out-sym) ,dim))
                           ,row)))
           finally (return ,(gk p :out-sym)))))))

(defun do_@$fx (rec b p &aux (dim (gk p :dim)) (ty (gk p :ty)) (aty (gk p :aty)))
  (let ((p (vchain (#'sliceconf #'lconf #'symconf) p b))
        (row `(,(gk p :fx* ) ,@(loop for d from 0 repeat dim
                                     collect `(:vr lft ,d)))))
    `(loop with ,(gk p :lft-sym) of-type ,aty = ,(funcall rec (gk p :lft))
           ,@(vec-select-itr p)
           do ,(-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                 `(($nvset (,(gk p :out-sym) ,dim (* ,(gk p :itr-out-sym) ,dim))
                           ,row)))
           finally (return ,(gk p :out-sym)))))

; -- ARRAY LEFT vec ----------------------------------------------------------

(defun do!@$fx (rec b p &aux (dim (gk p :dim)) (ty (gk p :ty)) (aty (gk p :aty)))
  (let ((p (vchain (#'sliceconf #'lconf #'symconf) p b))
        (row (loop for d from 0 repeat dim
                   collect `(,(gk p :fx*) (:vr lft ,d) (:vr rht ,d)))))
    (-vmvb* ty dim 'rht `(~ ,@(funcall rec (subseq b 2)))
      `((loop with ,(gk p :lft-sym) of-type ,aty = ,(funcall rec (gk p :lft))
              ,@(vec-select-itr p)
              do ,(-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                    `(($nvset (,(gk p :out-sym) ,dim (* ,(gk p :itr-out-sym) ,dim))
                              (values ,@row))))
              finally (return ,(gk p :out-sym)))))))

(defun do!@$fx. (rec b p &aux (dim (gk p :dim)) (ty (gk p :ty)) (aty (gk p :aty)))
  (let ((p (vchain (#'sliceconf #'lconf #'symconf) p b))
        (row (loop with rhs = (nvrs 'rht 0 (gk p :dots))
                   for d from 0 repeat dim
                   collect `(,(gk p :fx*) (:vr lft ,d) ,@rhs))))
    (-vmvb* ty (gk p :dots) 'rht `(~ ,@(funcall rec (subseq b 2)))
      `((loop with ,(gk p :lft-sym) of-type ,aty = ,(funcall rec (gk p :lft))
              ,@(vec-select-itr p)
              do ,(-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                    `(($nvset (,(gk p :out-sym) ,dim (* ,(gk p :itr-out-sym) ,dim))
                              (values ,@row))))
              finally (return ,(gk p :out-sym)))))))

; -- ARRAYS LEFT RIGHT vec ---------------------------------------------------

(defun do!@$fx$ (rec b p &aux (dim (gk p :dim)) (ty (gk p :ty)) (aty (gk p :aty)))
  (let ((p (vchain (#'sliceconf #'lconf #'rconf #'symconf) p b))
        (row (loop for d from 0 repeat dim
                   collect `(,(gk p :fx*) (:vr lft ,d) (:vr rht ,d)))))
    `(loop with ,(gk p :rht-sym) of-type ,aty = ,(funcall rec (gk p :rht))
           with ,(gk p :lft-sym) of-type ,aty = ,(funcall rec (gk p :lft))
          ,@(vec-select-itr p)
           do ,(-vmvb* ty dim 'rht ($row p :itr-rht-sym :rht-sym)
                 `(,(-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                      `(($nvset (,(gk p :out-sym) ,dim (* ,(gk p :itr-out-sym) ,dim))
                                (values ,@row))))))
           finally (return ,(gk p :out-sym)))))

(defmacro vv (&body body)
  "DSL for vectorized ops on packs of (values ...)

  type prefixes i: in, p: pn, f: ff, d: df

  (i2!@fx 1 2 3 4) -> (fx 1 3) | (fx 2 4)

  (i2!@fx. 1 2 3) -> (fx 1 3) | (fx 2 3)
  (i2!@.fx 1 2 3) -> (fx 1 2) | (fx 1 3)

  (i2!@fx.. 1 2 3 4) -> (fx 1 3 4) | (fx 2 3 4)
  (i2!@..fx 1 2 3 4) -> (fx 1 2 3) | (fx 1 2 4)

  (i2!@$fx #(1 2 3 4) 5 6) -> ((fx 1 5) (fx 2 6)
                               (fx 3 5) (fx 4 6))

  (i2!@$fx. #(1 2 3 4) 5) -> ((fx 1 5) (fx 2 5)
                              (fx 3 5) (fx 4 5))

  (i2!@$fx$ #(1 2 3 4) #(5 6 7 8)) -> ((fx 1 5) (fx 2 6)
                                       (fx 3 7) (fx 4 8))

  (i_@$fx #(1 2)) -> (fx 1) -> (x1)
                     (fx 2)    (x2)

  (i2_@$fx #(1 2 3 4)) -> (fx 1 2) -> (x1 y1)
                          (fx 3 4)    (x2 y2)

  ; (i2>1_@$fx #(1 2 3 4)) -> (fx 1 2) -> (x1)
  ;                           (fx 3 4)    (x2)

  (i2_@$fx$ #(1 2 3 4) #(5 6 7 8)) -> (fx 1 2 5 6) -> (x1 y1)
                                      (fx 3 4 7 8)    (x2 y2)"
  (labels
    ((err (p b msg) (error (format nil "VV: ~a, for: ~s~%in: ~s " msg (gk p :fx) b)))

    (do-mvc-tx (b &aux (p (vvconf b :vv-sym *vv-mvc*))) (dom@fx #'rec b p))

    (do-1ary-tx (b &aux (p (vvconf b :vv-sym *vv-1ary*))) ;.@
      (cond
        ((gk0 p :$l :$r :.l :.r) (do.@fx #'rec b p))
        ((gk+ p :$l)
         (unless (= 2 (length b)) (err p b ".@: incorrect number of elements"))
         (do.@$fx #'rec b p))
        (t (err p b ".@: unexpected input"))))

    (do-nary-tx (b &aux (p (vvconf b :vv-sym *vv-nary*))) ; _@
      (cond
        ((> (gk p :arrs) 1) (err p b "_@: invalid input, too many $"))
        ((gk+ p :.l :.r) (err p b "_@: vec broadcasting on both sides"))

        ((gk+ p :$l :.r)
         ; (unless (> (length b) 2) (err p b "!@: missing vecs"))
         (do_@$fx. #'rec b p))

        ((gk+ p :$l)
         (unless (= 2 (length b)) (err p b "_@: incorrect number of elements"))
         (do_@$fx #'rec b p))

        ((gk0 p :$l :$r :.l :.r) (do_@fx #'rec b p))
        (t (err p b "_@: unexpected input"))))

    ; TODO: (if ! dim must be == dimout)
    (do-vec-tx (b &aux (p (vvconf b))) ; !@
      (cond
        ((> (gk p :arrs) 1) (err p b "!@: invalid input, too many $"))
        ((gk+ p :.l :.r) (err p b "!@: vec broadcasting on both sides"))
        ((gk+ p :.l :$r) (err p b "!@: not implemented")) ; skip .fx$

        ((gk+ p :$l :$r)
         (unless (= 3 (length b)) (err p b "!@: incorrect number of elements"))
         (do!@$fx$ #'rec b p))

        ((gk+ p :$l :.r)
         (unless (> (length b) 2) (err p b "!@: missing vecs"))
         (do!@$fx. #'rec b p))
        ((gk+ p :$r) (err p b "!@: not implemented")) ; skip fx$

        ((gk+ p :$l)
         (unless (> (length b) 2) (err p b "!@: missing vecs"))
         (do!@$fx #'rec b p))
        ((gk+ p :.r) (do!@fx. #'rec b p))
        ((gk+ p :.l) (do!@.fx #'rec b p))

        ((gk0 p :$l :$r :.l :.r) (do!@fx #'rec b p))
        (t (err p b "!@: unexpected input"))))
    (rec (b)
      (cond ((atom b) b)

            ((and (listp b) (symbolp (car b))
                  (match-substr (mkstr *vv-sym*) (mkstr (car b))))
               (do-vec-tx b))
            ((and (listp b) (symbolp (car b))
                  (match-substr (mkstr *vv-1ary*) (mkstr (car b))))
               (do-1ary-tx b))
            ((and (listp b) (symbolp (car b))
                  (match-substr (mkstr *vv-nary*) (mkstr (car b))))
               (do-nary-tx b))

            ((and (listp b) (symbolp (car b))
                  (match-substr (mkstr *vv-mvc*) (mkstr (car b))))
             (do-mvc-tx b))

            ((consp b) (cons (rec (car b)) (rec (cdr b))))
            (t (error "VV: unexpected expr in: ~a" b)))))
    `(progn ,@(replace-varg (rec body)))))

