
(defpackage #:veq
  (:use #:common-lisp)
  (:export
    #:*eps*

    #:mac #:mac*
    #:ffl #:dfl

    #:dpi #:dpi5 #:dpii #:fpi #:fpi5 #:fpii
    #:df #:df* #:dvec #:ff #:ff* #:fvec
    #:in #:in* #:ivec

    #:fclamp #:dclamp

    #:vdef #:vdef* #:def* #:fvdef* #:fvdef
    #:vprogn #:fvprogn #:varg #:vref

    #:from-lst #:lst #:mvb #:mvc #:mvcwrap #:dsb #:vgrp-mvc

    #:d2let #:d2mvb #:d3let #:d3mvb
    #:f2let #:f2mvb #:f3let #:f3mvb

    #:d$_ #:d_ #:f$_ #:f_
    #:$ #:2$ #:3$ #:4$

    #:$print #:2$print #:3$print #:4$print #:vpr
    #:$num #:2$num #:3$num
    #:$to-list #:2$to-list #:3$to-list #:4to-list

    #:d$make #:d$one #:d$val #:d$zero #:d$copy #:d$last
    #:f$make #:f$one #:f$val #:f$zero #:f$copy #:f$last

    #:d2$make #:d2$one #:d2$val #:d2$zero #:d2$copy #:d2$last
    #:f2$make #:f2$one #:f2$val #:f2$zero #:f2$copy #:f2$last

    #:d3$make #:d3$one #:d3$val #:d3$zero #:d3$copy #:d3$last
    #:f3$make #:f3$one #:f3$val #:f3$zero #:f3$copy #:f3$last

    #:d4$make #:d4$one #:d4$val #:d4$zero #:d4$copy #:d4$last
    #:f4$make #:f4$one #:f4$val #:f4$zero #:f4$copy #:f4$last

    #:d$line #:d$point #:d2$line #:d2$point
    #:d3$line #:d3$point #:d4$line #:d4$point

    #:f$line #:f$point #:f2$line #:f2$point
    #:f3$line #:f3$point #:f4$line #:f4$point

    #:d2$center #:d2$circ #:d2$polygon #:d2$rect #:d2$square
    #:f2$center #:f2$circ #:f2$polygon #:f2$rect #:f2$square

    #:f2in-triangle #:f2in-bbox #:f2in-concave
    #:f2segdst #:f2lsegx #:f2segx
    #:f3planex

    #:vprod #:vsum))

