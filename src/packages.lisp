
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

    #:arr
    #:varg
    #:vdef
    #:vdef*
    #:vprogn
    #:vref

    #:lst
    #:mac
    #:mvb
    #:mvc

    #:d2mvb
    #:d3mvb
    #:f2mvb
    #:f3mvb
    #:d2let
    #:d3let
    #:f2let
    #:f3let

    #:to-list ; TODO: rename?
    #:2to-list
    #:3to-list

    #:$len
    #:2$len
    #:2vprint
    #:3$len
    #:3vprint
    #:d$_
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
    #:d_ ; TODO: rename
    #:f$_
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
    #:f_ ; TODO: rename
    #:vprint

    #:f2inside-bbox
    #:f2inside-concave
    #:f2lsegx
    #:f2segdst
    #:f2segx
    #:f3planex))

