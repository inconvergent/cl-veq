
(defpackage #:veq
  (:use #:common-lisp)
  (:export
    #:*eps*
    #:df
    #:df*
    #:dpi
    #:dpi5
    #:dpii
    #:dvec
    #:ff
    #:ff*
    #:fpi
    #:fpi5
    #:fpii
    #:fvec
    #:in
    #:in*
    #:ivec

    #:fclamp
    #:dclamp

    #:arr
    #:ivdef
    #:ivdef*
    #:varg
    #:vdef
    #:vdef*
    #:vprogn
    #:vref

    #:lst
    #:mac
    #:mvb
    #:mvc
    #:mvcwrap

    #:d2let
    #:d2mvb
    #:d3let
    #:d3mvb
    #:f2let
    #:f2mvb
    #:f3let
    #:f3mvb

    #:2to-list
    #:3to-list
    #:to-list

    #:$num
    #:2$num
    #:3$num

    #:$print
    #:2$print
    #:3$print
    #:vpr

    #:d$_
    #:d_ ; TODO: rename
    #:f$_
    #:f_ ; TODO: rename

    #:d$copy
    #:d$last
    #:d$make
    #:d$one
    #:d$val
    #:d$zero

    #:d2$last
    #:d2$one
    #:d2$val
    #:d2$zero
    #:d3$last
    #:d3$one
    #:d3$val
    #:d3$zero

    #:f$copy
    #:f$last
    #:f$make
    #:f$one
    #:f$val
    #:f$zero

    #:f2$center
    #:f2$circ
    #:f2$last
    #:f2$one
    #:f2$polygon
    #:f2$rect
    #:f2$square
    #:f2$val
    #:f2$zero

    #:f3$last
    #:f3$one
    #:f3$val
    #:f3$zero

    #:f2inside-bbox
    #:f2inside-concave
    #:f2lsegx
    #:f2segdst
    #:f2segx
    #:f3planex

    #:vprod #:vsum

    #:x #:y #:z
    #:xy #:xz
    #:yz #:yx
    #:zy #:zx
    #:xzy #:xyz
    #:zxy #:zyx
    #:yxz #:yzx))

