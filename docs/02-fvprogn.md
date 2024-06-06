# FVPROGN / VPROGN CONTEXT

## `d`
```
strict make 1d vector in veq context.
```

## `d$fxlspace`
```
args: ((n a b &key (end t)) &body fx)
for 1d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D$FXLSPACE (n a b) (lambda (i (:va 1 a b)) (vpr i a b)))
```

## `d$s`
```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 1
```

## `d2`
```
strict make 2d vector in veq context.
```

## `d2$fxlspace`
```
args: ((n a b &key (end t)) &body fx)
for 2d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D2$FXLSPACE (n a b) (lambda (i (:va 2 a b)) (vpr i a b)))
```

## `d2$s`
```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D2$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 2
```

## `d2angle`
```
veq context op: d2angle
fxname: -d2angle
args: (ax ay)
body (1): (atan ay ax).
```

## `d2cross`
```
veq context op: d2cross
fxname: -d2cross
args: (ax ay bx by)
body (2): (- (* ax by) (* ay bx)).
```

## `d2dot`
```
veq context op: d2dot
fxname: -d2dot
args: (ax ay bx by)
body (1): (+ (* ax bx) (* ay by)).
```

## `d2dst`
```
veq context op: d2dst
fxname: -d2dst
args: (ax ay bx by)
body (1): (sqrt (the pos-df (mvc #'+ (-d2square (- bx ax) (- by ay))))).
```

## `d2dst2`
```
veq context op: d2dst2
fxname: -d2dst2
args: (ax ay bx by)
body (1): (mvc #'+ (-d2square (- bx ax) (- by ay))).
```

## `d2flip`
```
veq context op: d2flip
fxname: -d2flip
args: (ax ay)
body (2): (values ay ax).
```

## `d2from`
```
veq context op: d2from
fxname: -d2from
args: (ax ay bx by s)
body (2): (values (+ ax (* bx s)) (+ ay (* by s))).
```

## `d2i-`
```
veq context op: d2i-
fxname: -d2i-
args: (ax ay bx by)
body (2): (values (- bx ax) (- by ay)).
```

## `d2i/`
```
veq context op: d2i/
fxname: -d2i/
args: (ax ay bx by)
body (2): (values (/ bx ax) (/ by ay)).
```

## `d2id`
```
veq context op: d2id
fxname: -d2id
args: (ax ay)
body (2): (values ax ay).
```

## `d2iscale`
```
veq context op: d2iscale
fxname: -d2iscale
args: (ax ay s)
body (2): (values (/ ax s) (/ ay s)).
```

## `d2len`
```
veq context op: d2len
fxname: -d2len
args: (ax ay)
body (1): (the pos-df (sqrt (the pos-df (mvc #'+ (-d2square ax ay))))).
```

## `d2len2`
```
veq context op: d2len2
fxname: -d2len2
args: (ax ay)
body (1): (the pos-df (mvc #'+ (-d2square ax ay))).
```

## `d2lerp`
```
veq context op: d2lerp
fxname: -d2lerp
args: (ax ay bx by s)
body (2): (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))).
```

## `d2let`
```
make 2d let.
ex: (D2LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

## `d2max`
```
veq context op: d2max
fxname: -d2max
args: (ax ay)
body (1): (max ax ay).
```

## `d2mid`
```
veq context op: d2mid
fxname: -d2mid
args: (ax ay bx by)
body (2): (values (* (+ ax bx) 0.5d0) (* (+ ay by) 0.5d0)).
```

## `d2min`
```
veq context op: d2min
fxname: -d2min
args: (ax ay)
body (1): (min ax ay).
```

## `d2norm`
```
veq context op: d2norm
fxname: -d2norm
args: (ax ay)
body (2): (mvc #'-d2iscale ax ay (mvc #'-d2len ax ay)).
```

## `d2on-circ`
```
veq context op: d2on-circ
fxname: -d2on-circ
args: (ax rad)
body (2): (mvc #'-d2scale (-dcos-sin (* ax dpii)) rad).
```

## `d2on-circ*`
```
veq context op: d2on-circ*
fxname: -d2on-circ*
args: (ax rad)
body (2): (mvc #'-d2scale (-dcos-sin ax) rad).
```

## `d2perp`
```
veq context op: d2perp
fxname: -d2perp
args: (ax ay)
body (2): (values ay (- ax)).
```

## `d2perp*`
```
veq context op: d2perp*
fxname: -d2perp*
args: (ax ay)
body (2): (values (- ay) ax).
```

## `d2rep`
```
repeat argument 2d times as values.
ex: (D2REP (fx)) corresponds to (values (fx) ...).
```

## `d2rot`
```
veq context op: d2rot
fxname: -d2rot
args: (ax ay angle)
body (2): (let ((cosa (cos angle)) (sina (sin angle)))
            (declare (df cosa sina))
            (values (- (* ax cosa) (* ay sina)) (+ (* ax sina) (* ay cosa)))).
```

## `d2rots`
```
veq context op: d2rots
fxname: -d2rots
args: (ax ay angle sx sy)
body (2): (mvb (rx ry) (mvc #'-d2rot (- ax sx) (- ay sy) angle)
               (values (+ sx rx) (+ sy ry))).
```

## `d2scale`
```
veq context op: d2scale
fxname: -d2scale
args: (ax ay s)
body (2): (values (* ax s) (* ay s)).
```

## `d2square`
```
veq context op: d2square
fxname: -d2square
args: (ax ay)
body (2): (values (* ax ax) (* ay ay)).
```

## `d2val`
```
repeat the evaluated argument 2 times as values.
ex: (D2VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `d2~`
```
make 2d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `d3`
```
strict make 3d vector in veq context.
```

## `d3$fxlspace`
```
args: ((n a b &key (end t)) &body fx)
for 3d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D3$FXLSPACE (n a b) (lambda (i (:va 3 a b)) (vpr i a b)))
```

## `d3$s`
```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D3$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 3
```

## `d3cross`
```
veq context op: d3cross
fxname: -d3cross
args: (ax ay az bx by bz)
body (3): (values (- (* ay bz) (* az by)) (- (* az bx) (* ax bz))
                  (- (* ax by) (* ay bx))).
```

## `d3dot`
```
veq context op: d3dot
fxname: -d3dot
args: (ax ay az bx by bz)
body (1): (+ (* ax bx) (* ay by) (* az bz)).
```

## `d3dst`
```
veq context op: d3dst
fxname: -d3dst
args: (ax ay az bx by bz)
body (1): (sqrt
           (the pos-df (mvc #'+ (-d3square (- bx ax) (- by ay) (- bz az))))).
```

## `d3dst2`
```
veq context op: d3dst2
fxname: -d3dst2
args: (ax ay az bx by bz)
body (1): (mvc #'+ (-d3square (- bx ax) (- by ay) (- bz az))).
```

## `d3from`
```
veq context op: d3from
fxname: -d3from
args: (ax ay az bx by bz s)
body (3): (values (+ ax (* bx s)) (+ ay (* by s)) (+ az (* bz s))).
```

## `d3i-`
```
veq context op: d3i-
fxname: -d3i-
args: (ax ay az bx by bz)
body (3): (values (- bx ax) (- by ay) (- bz az)).
```

## `d3i/`
```
veq context op: d3i/
fxname: -d3i/
args: (ax ay az bx by bz)
body (3): (values (/ bx ax) (/ by ay) (/ bz az)).
```

## `d3id`
```
veq context op: d3id
fxname: -d3id
args: (ax ay az)
body (3): (values ax ay az).
```

## `d3iscale`
```
veq context op: d3iscale
fxname: -d3iscale
args: (ax ay az s)
body (3): (values (/ ax s) (/ ay s) (/ az s)).
```

## `d3len`
```
veq context op: d3len
fxname: -d3len
args: (ax ay az)
body (1): (the pos-df (sqrt (the pos-df (mvc #'+ (-d3square ax ay az))))).
```

## `d3len2`
```
veq context op: d3len2
fxname: -d3len2
args: (ax ay az)
body (1): (the pos-df (mvc #'+ (-d3square ax ay az))).
```

## `d3lerp`
```
veq context op: d3lerp
fxname: -d3lerp
args: (ax ay az bx by bz s)
body (3): (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))
                  (+ az (* (- bz az) s))).
```

## `d3let`
```
make 3d let.
ex: (D3LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

## `d3max`
```
veq context op: d3max
fxname: -d3max
args: (ax ay az)
body (1): (max ax ay az).
```

## `d3mid`
```
veq context op: d3mid
fxname: -d3mid
args: (ax ay az bx by bz)
body (3): (values (* (+ ax bx) 0.5d0) (* (+ ay by) 0.5d0) (* (+ az bz) 0.5d0)).
```

## `d3min`
```
veq context op: d3min
fxname: -d3min
args: (ax ay az)
body (1): (min ax ay az).
```

## `d3norm`
```
veq context op: d3norm
fxname: -d3norm
args: (ax ay az)
body (3): (mvc #'-d3iscale ax ay az (the pos-df (mvc #'-d3len ax ay az))).
```

## `d3rep`
```
repeat argument 3d times as values.
ex: (D3REP (fx)) corresponds to (values (fx) ...).
```

## `d3rot`
```
veq context op: d3rot
fxname: -d3rot
args: (ax ay az nx ny nz a)
body (3): (let ((cosa (cos a)))
            (declare (df cosa))
            (mvc #'-d3from
                 (mvc #'-d3from (-d3scale ax ay az cosa)
                      (-d3cross nx ny nz ax ay az) (sin a))
                 nx ny nz (* (-d3dot nx ny nz ax ay az) (- 1.0d0 cosa)))).
```

## `d3rots`
```
veq context op: d3rots
fxname: -d3rots
args: (ax ay az nx ny nz a sx sy sz)
body (3): (mvb (rx ry rz)
               (mvc #'-d3rot (- ax sx) (- ay sy) (- az sz) nx ny nz a)
               (values (+ (the df rx) sx) (+ (the df ry) sy)
                       (+ (the df rz) sz))).
```

## `d3scale`
```
veq context op: d3scale
fxname: -d3scale
args: (ax ay az s)
body (3): (values (* ax s) (* ay s) (* az s)).
```

## `d3square`
```
veq context op: d3square
fxname: -d3square
args: (ax ay az)
body (3): (values (the pos-df (* ax ax)) (the pos-df (* ay ay))
                  (the pos-df (* az az))).
```

## `d3val`
```
repeat the evaluated argument 3 times as values.
ex: (D3VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `d3~`
```
make 3d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `d4`
```
strict make 4d vector in veq context.
```

## `d4$fxlspace`
```
args: ((n a b &key (end t)) &body fx)
for 4d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D4$FXLSPACE (n a b) (lambda (i (:va 4 a b)) (vpr i a b)))
```

## `d4$s`
```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D4$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 4
```

## `d4dot`
```
veq context op: d4dot
fxname: -d4dot
args: (ax ay az aw bx by bz bw)
body (1): (+ (* ax bx) (* ay by) (* az bz) (* aw bw)).
```

## `d4dst`
```
veq context op: d4dst
fxname: -d4dst
args: (ax ay az aw bx by bz bw)
body (1): (sqrt
           (the pos-df
                (mvc #'+ (-d4square (- bx ax) (- by ay) (- bz az) (- bw aw))))).
```

## `d4dst2`
```
veq context op: d4dst2
fxname: -d4dst2
args: (ax ay az aw bx by bz bw)
body (1): (mvc #'+ (-d4square (- bx ax) (- by ay) (- bz az) (- bw aw))).
```

## `d4from`
```
veq context op: d4from
fxname: -d4from
args: (ax ay az aw bx by bz bw s)
body (4): (values (+ ax (* bx s)) (+ ay (* by s)) (+ az (* bz s))
                  (+ aw (* bw s))).
```

## `d4i-`
```
veq context op: d4i-
fxname: -d4i-
args: (ax ay az aw bx by bz bw)
body (4): (values (- bx ax) (- by ay) (- bz az) (- bw aw)).
```

## `d4i/`
```
veq context op: d4i/
fxname: -d4i/
args: (ax ay az aw bx by bz bw)
body (4): (values (/ bx ax) (/ by ay) (/ bz az) (/ bw aw)).
```

## `d4id`
```
veq context op: d4id
fxname: -d4id
args: (ax ay az aw)
body (4): (values ax ay az aw).
```

## `d4iscale`
```
veq context op: d4iscale
fxname: -d4iscale
args: (ax ay az aw s)
body (4): (values (/ ax s) (/ ay s) (/ az s) (/ aw s)).
```

## `d4len`
```
veq context op: d4len
fxname: -d4len
args: (ax ay az aw)
body (1): (the pos-df (sqrt (the pos-df (mvc #'+ (-d4square ax ay az aw))))).
```

## `d4len2`
```
veq context op: d4len2
fxname: -d4len2
args: (ax ay az aw)
body (1): (the pos-df (mvc #'+ (-d4square ax ay az aw))).
```

## `d4lerp`
```
veq context op: d4lerp
fxname: -d4lerp
args: (ax ay az aw bx by bz bw s)
body (4): (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))
                  (+ az (* (- bz az) s)) (+ aw (* (- bw aw) s))).
```

## `d4let`
```
make 4d let.
ex: (D4LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

## `d4max`
```
veq context op: d4max
fxname: -d4max
args: (ax ay az aw)
body (1): (max ax ay az aw).
```

## `d4mid`
```
veq context op: d4mid
fxname: -d4mid
args: (ax ay az aw bx by bz bw)
body (4): (values (* (+ ax bx) 0.5d0) (* (+ ay by) 0.5d0) (* (+ az bz) 0.5d0)
                  (* (+ aw bw) 0.5d0)).
```

## `d4min`
```
veq context op: d4min
fxname: -d4min
args: (ax ay az aw)
body (1): (min ax ay az aw).
```

## `d4norm`
```
veq context op: d4norm
fxname: -d4norm
args: (ax ay az aw)
body (4): (mvc #'-d4iscale ax ay az aw (the pos-df (mvc #'-d4len ax ay az aw))).
```

## `d4rep`
```
repeat argument 4d times as values.
ex: (D4REP (fx)) corresponds to (values (fx) ...).
```

## `d4scale`
```
veq context op: d4scale
fxname: -d4scale
args: (ax ay az aw s)
body (4): (values (* ax s) (* ay s) (* az s) (* aw s)).
```

## `d4square`
```
veq context op: d4square
fxname: -d4square
args: (ax ay az aw)
body (4): (values (the pos-df (* ax ax)) (the pos-df (* ay ay))
                  (the pos-df (* az az)) (the pos-df (* aw aw))).
```

## `d4val`
```
repeat the evaluated argument 4 times as values.
ex: (D4VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `d4~`
```
make 4d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `dclamp`
```
veq context op: dclamp
fxname: -dclamp
args: (x)
body (1): (min 1.0d0 (max 0.0d0 x)).
```

## `dclamp*`
```
veq context op: dclamp*
fxname: -dclamp*
args: (x mi ma)
body (1): (min ma (max mi x)).
```

## `dcos-sin`
```
veq context op: dcos-sin
fxname: -dcos-sin
args: (ax)
body (2): (values (cos ax) (sin ax)).
```

## `ddeg->rad`
```
veq context op: ddeg->rad
fxname: -ddeg->rad
args: (d)
body (1): (* dpi (/ d 180.0d0)).
```

## `dfrom`
```
veq context op: dfrom
fxname: -dfrom
args: (ax bx s)
body (1): (+ ax (* bx s)).
```

## `di-`
```
veq context op: di-
fxname: -di-
args: (ax bx)
body (1): (- bx ax).
```

## `di/`
```
veq context op: di/
fxname: -di/
args: (ax bx)
body (1): (/ bx ax).
```

## `did`
```
veq context op: did
fxname: -did
args: (ax)
body (1): (values ax).
```

## `discale`
```
veq context op: discale
fxname: -discale
args: (ax s)
body (1): (/ ax s).
```

## `dlen`
```
veq context op: dlen
fxname: -dlen
args: (ax)
body (1): (the pos-df ax).
```

## `dlen2`
```
veq context op: dlen2
fxname: -dlen2
args: (ax)
body (1): (the pos-df (mvc #'+ (-dsquare ax))).
```

## `dlerp`
```
veq context op: dlerp
fxname: -dlerp
args: (ax bx s)
body (1): (+ ax (* (- bx ax) s)).
```

## `dmid`
```
veq context op: dmid
fxname: -dmid
args: (ax bx)
body (1): (* (+ ax bx) 0.5d0).
```

## `drep`
```
repeat argument 1d times as values.
ex: (DREP (fx)) corresponds to (values (fx) ...).
```

## `dscale`
```
veq context op: dscale
fxname: -dscale
args: (ax s)
body (1): (* ax s).
```

## `dsin-cos`
```
veq context op: dsin-cos
fxname: -dsin-cos
args: (ax)
body (2): (values (sin ax) (cos ax)).
```

## `dsquare`
```
veq context op: dsquare
fxname: -dsquare
args: (ax)
body (1): (* ax ax).
```

## `dval`
```
repeat the evaluated argument 1 times as values.
ex: (DVAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `d~`
```
make 1d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `f`
```
strict make 1d vector in veq context.
```

## `f$fxlspace`
```
args: ((n a b &key (end t)) &body fx)
for 1d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F$FXLSPACE (n a b) (lambda (i (:va 1 a b)) (vpr i a b)))
```

## `f$s`
```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 1
```

## `f2`
```
strict make 2d vector in veq context.
```

## `f2$fxlspace`
```
args: ((n a b &key (end t)) &body fx)
for 2d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F2$FXLSPACE (n a b) (lambda (i (:va 2 a b)) (vpr i a b)))
```

## `f2$s`
```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F2$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 2
```

## `f2angle`
```
veq context op: f2angle
fxname: -f2angle
args: (ax ay)
body (1): (atan ay ax).
```

## `f2cross`
```
veq context op: f2cross
fxname: -f2cross
args: (ax ay bx by)
body (2): (- (* ax by) (* ay bx)).
```

## `f2dot`
```
veq context op: f2dot
fxname: -f2dot
args: (ax ay bx by)
body (1): (+ (* ax bx) (* ay by)).
```

## `f2dst`
```
veq context op: f2dst
fxname: -f2dst
args: (ax ay bx by)
body (1): (sqrt (the pos-ff (mvc #'+ (-f2square (- bx ax) (- by ay))))).
```

## `f2dst2`
```
veq context op: f2dst2
fxname: -f2dst2
args: (ax ay bx by)
body (1): (mvc #'+ (-f2square (- bx ax) (- by ay))).
```

## `f2flip`
```
veq context op: f2flip
fxname: -f2flip
args: (ax ay)
body (2): (values ay ax).
```

## `f2from`
```
veq context op: f2from
fxname: -f2from
args: (ax ay bx by s)
body (2): (values (+ ax (* bx s)) (+ ay (* by s))).
```

## `f2i-`
```
veq context op: f2i-
fxname: -f2i-
args: (ax ay bx by)
body (2): (values (- bx ax) (- by ay)).
```

## `f2i/`
```
veq context op: f2i/
fxname: -f2i/
args: (ax ay bx by)
body (2): (values (/ bx ax) (/ by ay)).
```

## `f2id`
```
veq context op: f2id
fxname: -f2id
args: (ax ay)
body (2): (values ax ay).
```

## `f2iscale`
```
veq context op: f2iscale
fxname: -f2iscale
args: (ax ay s)
body (2): (values (/ ax s) (/ ay s)).
```

## `f2len`
```
veq context op: f2len
fxname: -f2len
args: (ax ay)
body (1): (the pos-ff (sqrt (the pos-ff (mvc #'+ (-f2square ax ay))))).
```

## `f2len2`
```
veq context op: f2len2
fxname: -f2len2
args: (ax ay)
body (1): (the pos-ff (mvc #'+ (-f2square ax ay))).
```

## `f2lerp`
```
veq context op: f2lerp
fxname: -f2lerp
args: (ax ay bx by s)
body (2): (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))).
```

## `f2let`
```
make 2d let.
ex: (F2LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

## `f2max`
```
veq context op: f2max
fxname: -f2max
args: (ax ay)
body (1): (max ax ay).
```

## `f2mid`
```
veq context op: f2mid
fxname: -f2mid
args: (ax ay bx by)
body (2): (values (* (+ ax bx) 0.5) (* (+ ay by) 0.5)).
```

## `f2min`
```
veq context op: f2min
fxname: -f2min
args: (ax ay)
body (1): (min ax ay).
```

## `f2norm`
```
veq context op: f2norm
fxname: -f2norm
args: (ax ay)
body (2): (mvc #'-f2iscale ax ay (mvc #'-f2len ax ay)).
```

## `f2on-circ`
```
veq context op: f2on-circ
fxname: -f2on-circ
args: (ax rad)
body (2): (mvc #'-f2scale (-fcos-sin (* ax fpii)) rad).
```

## `f2on-circ*`
```
veq context op: f2on-circ*
fxname: -f2on-circ*
args: (ax rad)
body (2): (mvc #'-f2scale (-fcos-sin ax) rad).
```

## `f2perp`
```
veq context op: f2perp
fxname: -f2perp
args: (ax ay)
body (2): (values ay (- ax)).
```

## `f2perp*`
```
veq context op: f2perp*
fxname: -f2perp*
args: (ax ay)
body (2): (values (- ay) ax).
```

## `f2rep`
```
repeat argument 2d times as values.
ex: (F2REP (fx)) corresponds to (values (fx) ...).
```

## `f2rot`
```
veq context op: f2rot
fxname: -f2rot
args: (ax ay angle)
body (2): (let ((cosa (cos angle)) (sina (sin angle)))
            (declare (ff cosa sina))
            (values (- (* ax cosa) (* ay sina)) (+ (* ax sina) (* ay cosa)))).
```

## `f2rots`
```
veq context op: f2rots
fxname: -f2rots
args: (ax ay angle sx sy)
body (2): (mvb (rx ry) (mvc #'-f2rot (- ax sx) (- ay sy) angle)
               (values (+ sx rx) (+ sy ry))).
```

## `f2scale`
```
veq context op: f2scale
fxname: -f2scale
args: (ax ay s)
body (2): (values (* ax s) (* ay s)).
```

## `f2square`
```
veq context op: f2square
fxname: -f2square
args: (ax ay)
body (2): (values (* ax ax) (* ay ay)).
```

## `f2val`
```
repeat the evaluated argument 2 times as values.
ex: (F2VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `f2~`
```
make 2d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `f3`
```
strict make 3d vector in veq context.
```

## `f3$fxlspace`
```
args: ((n a b &key (end t)) &body fx)
for 3d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F3$FXLSPACE (n a b) (lambda (i (:va 3 a b)) (vpr i a b)))
```

## `f3$s`
```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F3$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 3
```

## `f3cross`
```
veq context op: f3cross
fxname: -f3cross
args: (ax ay az bx by bz)
body (3): (values (- (* ay bz) (* az by)) (- (* az bx) (* ax bz))
                  (- (* ax by) (* ay bx))).
```

## `f3dot`
```
veq context op: f3dot
fxname: -f3dot
args: (ax ay az bx by bz)
body (1): (+ (* ax bx) (* ay by) (* az bz)).
```

## `f3dst`
```
veq context op: f3dst
fxname: -f3dst
args: (ax ay az bx by bz)
body (1): (sqrt
           (the pos-ff (mvc #'+ (-f3square (- bx ax) (- by ay) (- bz az))))).
```

## `f3dst2`
```
veq context op: f3dst2
fxname: -f3dst2
args: (ax ay az bx by bz)
body (1): (mvc #'+ (-f3square (- bx ax) (- by ay) (- bz az))).
```

## `f3from`
```
veq context op: f3from
fxname: -f3from
args: (ax ay az bx by bz s)
body (3): (values (+ ax (* bx s)) (+ ay (* by s)) (+ az (* bz s))).
```

## `f3i-`
```
veq context op: f3i-
fxname: -f3i-
args: (ax ay az bx by bz)
body (3): (values (- bx ax) (- by ay) (- bz az)).
```

## `f3i/`
```
veq context op: f3i/
fxname: -f3i/
args: (ax ay az bx by bz)
body (3): (values (/ bx ax) (/ by ay) (/ bz az)).
```

## `f3id`
```
veq context op: f3id
fxname: -f3id
args: (ax ay az)
body (3): (values ax ay az).
```

## `f3iscale`
```
veq context op: f3iscale
fxname: -f3iscale
args: (ax ay az s)
body (3): (values (/ ax s) (/ ay s) (/ az s)).
```

## `f3len`
```
veq context op: f3len
fxname: -f3len
args: (ax ay az)
body (1): (the pos-ff (sqrt (the pos-ff (mvc #'+ (-f3square ax ay az))))).
```

## `f3len2`
```
veq context op: f3len2
fxname: -f3len2
args: (ax ay az)
body (1): (the pos-ff (mvc #'+ (-f3square ax ay az))).
```

## `f3lerp`
```
veq context op: f3lerp
fxname: -f3lerp
args: (ax ay az bx by bz s)
body (3): (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))
                  (+ az (* (- bz az) s))).
```

## `f3let`
```
make 3d let.
ex: (F3LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

## `f3max`
```
veq context op: f3max
fxname: -f3max
args: (ax ay az)
body (1): (max ax ay az).
```

## `f3mid`
```
veq context op: f3mid
fxname: -f3mid
args: (ax ay az bx by bz)
body (3): (values (* (+ ax bx) 0.5) (* (+ ay by) 0.5) (* (+ az bz) 0.5)).
```

## `f3min`
```
veq context op: f3min
fxname: -f3min
args: (ax ay az)
body (1): (min ax ay az).
```

## `f3norm`
```
veq context op: f3norm
fxname: -f3norm
args: (ax ay az)
body (3): (mvc #'-f3iscale ax ay az (the pos-ff (mvc #'-f3len ax ay az))).
```

## `f3rep`
```
repeat argument 3d times as values.
ex: (F3REP (fx)) corresponds to (values (fx) ...).
```

## `f3rot`
```
veq context op: f3rot
fxname: -f3rot
args: (ax ay az nx ny nz a)
body (3): (let ((cosa (cos a)))
            (declare (ff cosa))
            (mvc #'-f3from
                 (mvc #'-f3from (-f3scale ax ay az cosa)
                      (-f3cross nx ny nz ax ay az) (sin a))
                 nx ny nz (* (-f3dot nx ny nz ax ay az) (- 1.0 cosa)))).
```

## `f3rots`
```
veq context op: f3rots
fxname: -f3rots
args: (ax ay az nx ny nz a sx sy sz)
body (3): (mvb (rx ry rz)
               (mvc #'-f3rot (- ax sx) (- ay sy) (- az sz) nx ny nz a)
               (values (+ (the ff rx) sx) (+ (the ff ry) sy)
                       (+ (the ff rz) sz))).
```

## `f3scale`
```
veq context op: f3scale
fxname: -f3scale
args: (ax ay az s)
body (3): (values (* ax s) (* ay s) (* az s)).
```

## `f3square`
```
veq context op: f3square
fxname: -f3square
args: (ax ay az)
body (3): (values (the pos-ff (* ax ax)) (the pos-ff (* ay ay))
                  (the pos-ff (* az az))).
```

## `f3val`
```
repeat the evaluated argument 3 times as values.
ex: (F3VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `f3~`
```
make 3d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `f4`
```
strict make 4d vector in veq context.
```

## `f4$fxlspace`
```
args: ((n a b &key (end t)) &body fx)
for 4d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F4$FXLSPACE (n a b) (lambda (i (:va 4 a b)) (vpr i a b)))
```

## `f4$s`
```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F4$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 4
```

## `f4dot`
```
veq context op: f4dot
fxname: -f4dot
args: (ax ay az aw bx by bz bw)
body (1): (+ (* ax bx) (* ay by) (* az bz) (* aw bw)).
```

## `f4dst`
```
veq context op: f4dst
fxname: -f4dst
args: (ax ay az aw bx by bz bw)
body (1): (sqrt
           (the pos-ff
                (mvc #'+ (-f4square (- bx ax) (- by ay) (- bz az) (- bw aw))))).
```

## `f4dst2`
```
veq context op: f4dst2
fxname: -f4dst2
args: (ax ay az aw bx by bz bw)
body (1): (mvc #'+ (-f4square (- bx ax) (- by ay) (- bz az) (- bw aw))).
```

## `f4from`
```
veq context op: f4from
fxname: -f4from
args: (ax ay az aw bx by bz bw s)
body (4): (values (+ ax (* bx s)) (+ ay (* by s)) (+ az (* bz s))
                  (+ aw (* bw s))).
```

## `f4i-`
```
veq context op: f4i-
fxname: -f4i-
args: (ax ay az aw bx by bz bw)
body (4): (values (- bx ax) (- by ay) (- bz az) (- bw aw)).
```

## `f4i/`
```
veq context op: f4i/
fxname: -f4i/
args: (ax ay az aw bx by bz bw)
body (4): (values (/ bx ax) (/ by ay) (/ bz az) (/ bw aw)).
```

## `f4id`
```
veq context op: f4id
fxname: -f4id
args: (ax ay az aw)
body (4): (values ax ay az aw).
```

## `f4iscale`
```
veq context op: f4iscale
fxname: -f4iscale
args: (ax ay az aw s)
body (4): (values (/ ax s) (/ ay s) (/ az s) (/ aw s)).
```

## `f4len`
```
veq context op: f4len
fxname: -f4len
args: (ax ay az aw)
body (1): (the pos-ff (sqrt (the pos-ff (mvc #'+ (-f4square ax ay az aw))))).
```

## `f4len2`
```
veq context op: f4len2
fxname: -f4len2
args: (ax ay az aw)
body (1): (the pos-ff (mvc #'+ (-f4square ax ay az aw))).
```

## `f4lerp`
```
veq context op: f4lerp
fxname: -f4lerp
args: (ax ay az aw bx by bz bw s)
body (4): (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))
                  (+ az (* (- bz az) s)) (+ aw (* (- bw aw) s))).
```

## `f4let`
```
make 4d let.
ex: (F4LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

## `f4max`
```
veq context op: f4max
fxname: -f4max
args: (ax ay az aw)
body (1): (max ax ay az aw).
```

## `f4mid`
```
veq context op: f4mid
fxname: -f4mid
args: (ax ay az aw bx by bz bw)
body (4): (values (* (+ ax bx) 0.5) (* (+ ay by) 0.5) (* (+ az bz) 0.5)
                  (* (+ aw bw) 0.5)).
```

## `f4min`
```
veq context op: f4min
fxname: -f4min
args: (ax ay az aw)
body (1): (min ax ay az aw).
```

## `f4norm`
```
veq context op: f4norm
fxname: -f4norm
args: (ax ay az aw)
body (4): (mvc #'-f4iscale ax ay az aw (the pos-ff (mvc #'-f4len ax ay az aw))).
```

## `f4rep`
```
repeat argument 4d times as values.
ex: (F4REP (fx)) corresponds to (values (fx) ...).
```

## `f4scale`
```
veq context op: f4scale
fxname: -f4scale
args: (ax ay az aw s)
body (4): (values (* ax s) (* ay s) (* az s) (* aw s)).
```

## `f4square`
```
veq context op: f4square
fxname: -f4square
args: (ax ay az aw)
body (4): (values (the pos-ff (* ax ax)) (the pos-ff (* ay ay))
                  (the pos-ff (* az az)) (the pos-ff (* aw aw))).
```

## `f4val`
```
repeat the evaluated argument 4 times as values.
ex: (F4VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `f4~`
```
make 4d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `fclamp`
```
veq context op: fclamp
fxname: -fclamp
args: (x)
body (1): (min 1.0 (max 0.0 x)).
```

## `fclamp*`
```
veq context op: fclamp*
fxname: -fclamp*
args: (x mi ma)
body (1): (min ma (max mi x)).
```

## `fcos-sin`
```
veq context op: fcos-sin
fxname: -fcos-sin
args: (ax)
body (2): (values (cos ax) (sin ax)).
```

## `fdeg->rad`
```
veq context op: fdeg->rad
fxname: -fdeg->rad
args: (d)
body (1): (* fpi (/ d 180.0)).
```

## `ffrom`
```
veq context op: ffrom
fxname: -ffrom
args: (ax bx s)
body (1): (+ ax (* bx s)).
```

## `fi-`
```
veq context op: fi-
fxname: -fi-
args: (ax bx)
body (1): (- bx ax).
```

## `fi/`
```
veq context op: fi/
fxname: -fi/
args: (ax bx)
body (1): (/ bx ax).
```

## `fid`
```
veq context op: fid
fxname: -fid
args: (ax)
body (1): (values ax).
```

## `fiscale`
```
veq context op: fiscale
fxname: -fiscale
args: (ax s)
body (1): (/ ax s).
```

## `flen`
```
veq context op: flen
fxname: -flen
args: (ax)
body (1): (the pos-ff ax).
```

## `flen2`
```
veq context op: flen2
fxname: -flen2
args: (ax)
body (1): (the pos-ff (mvc #'+ (-fsquare ax))).
```

## `flerp`
```
veq context op: flerp
fxname: -flerp
args: (ax bx s)
body (1): (+ ax (* (- bx ax) s)).
```

## `fmid`
```
veq context op: fmid
fxname: -fmid
args: (ax bx)
body (1): (* (+ ax bx) 0.5).
```

## `frep`
```
repeat argument 1d times as values.
ex: (FREP (fx)) corresponds to (values (fx) ...).
```

## `fscale`
```
veq context op: fscale
fxname: -fscale
args: (ax s)
body (1): (* ax s).
```

## `fsin-cos`
```
veq context op: fsin-cos
fxname: -fsin-cos
args: (ax)
body (2): (values (sin ax) (cos ax)).
```

## `fsquare`
```
veq context op: fsquare
fxname: -fsquare
args: (ax)
body (1): (* ax ax).
```

## `fval`
```
repeat the evaluated argument 1 times as values.
ex: (FVAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `f~`
```
make 1d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `i`
```
strict make 1d vector in veq context.
```

## `i2`
```
strict make 2d vector in veq context.
```

## `i2let`
```
make 2d let.
ex: (I2LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

## `i2rep`
```
repeat argument 2d times as values.
ex: (I2REP (fx)) corresponds to (values (fx) ...).
```

## `i2val`
```
repeat the evaluated argument 2 times as values.
ex: (I2VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `i2~`
```
make 2d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `i3`
```
strict make 3d vector in veq context.
```

## `i3let`
```
make 3d let.
ex: (I3LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

## `i3rep`
```
repeat argument 3d times as values.
ex: (I3REP (fx)) corresponds to (values (fx) ...).
```

## `i3val`
```
repeat the evaluated argument 3 times as values.
ex: (I3VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `i3~`
```
make 3d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `i4`
```
strict make 4d vector in veq context.
```

## `i4let`
```
make 4d let.
ex: (I4LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

## `i4rep`
```
repeat argument 4d times as values.
ex: (I4REP (fx)) corresponds to (values (fx) ...).
```

## `i4val`
```
repeat the evaluated argument 4 times as values.
ex: (I4VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `i4~`
```
make 4d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `irep`
```
repeat argument 1d times as values.
ex: (IREP (fx)) corresponds to (values (fx) ...).
```

## `ival`
```
repeat the evaluated argument 1 times as values.
ex: (IVAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `i~`
```
make 1d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `p`
```
strict make 1d vector in veq context.
```

## `p2`
```
strict make 2d vector in veq context.
```

## `p2let`
```
make 2d let.
ex: (P2LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

## `p2rep`
```
repeat argument 2d times as values.
ex: (P2REP (fx)) corresponds to (values (fx) ...).
```

## `p2val`
```
repeat the evaluated argument 2 times as values.
ex: (P2VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `p2~`
```
make 2d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `p3`
```
strict make 3d vector in veq context.
```

## `p3let`
```
make 3d let.
ex: (P3LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

## `p3rep`
```
repeat argument 3d times as values.
ex: (P3REP (fx)) corresponds to (values (fx) ...).
```

## `p3val`
```
repeat the evaluated argument 3 times as values.
ex: (P3VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `p3~`
```
make 3d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `p4`
```
strict make 4d vector in veq context.
```

## `p4let`
```
make 4d let.
ex: (P4LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

## `p4rep`
```
repeat argument 4d times as values.
ex: (P4REP (fx)) corresponds to (values (fx) ...).
```

## `p4val`
```
repeat the evaluated argument 4 times as values.
ex: (P4VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `p4~`
```
make 4d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `prep`
```
repeat argument 1d times as values.
ex: (PREP (fx)) corresponds to (values (fx) ...).
```

## `pval`
```
repeat the evaluated argument 1 times as values.
ex: (PVAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

## `p~`
```
make 1d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

## `varg`
```
use (veq:varg dim a b ...) or (:vr dim a b ...) to represent dim vectors a,b
of dim n in vprogn, fvprog, fvdef*, vdef*, def*.  see replace-varg for
implementation details.
```

## `vref`
```
use (veq:vref s x) or (:vr s x) to get dim x of symbol s in vprogn,
fvprogn, fvdef*, vdef*, def*. see replace-varg for implementation details.
```

