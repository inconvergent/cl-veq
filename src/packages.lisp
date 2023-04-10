
(defpackage #:veq
  (:use #:common-lisp)
  (:export
    #:*eps*
    #:dpi #:dpi5 #:dpii #:dpi25
    #:fpi #:fpi5 #:fpii #:fpi25
    #:mac #:mac* #:v? #:d? #:i? #:context? #:ext-symbols?
    #:veqsymb

    #:from-lst #:lst #:mvb #:dsb
    #:mvc #:mvcwrap #:mvcgrp #:mvcmap #:~ #:$nvset

    #:pos-int #:pos-df #:pos-ff
    #:df #:df* #:ff #:ff* #:in #:in* #:pn #:pn*
    #:dvec #:fvec #:ivec #:pvec

    #:ffl #:dfl #:vsel

    #:vdef #:vdef* #:def* #:fvdef* #:fvdef #:vlabels
    #:vprogn #:fvprogn #:varg #:vref

    #:xlet #:vv

    #:new-stride
    #:$make #:$copy #:d$_ #:f$_ #:i_ #:p_
    #:$ #:2$ #:3$ #:4$
    #:f$ #:d$ #:f2$ #:d2$ #:f3$ #:d3$ #:f4$ #:d4$

    #:$print #:2$print #:3$print #:4$print #:vpr
    #:$num #:2$num #:3$num
    #:$to-list #:2$to-list #:3$to-list #:4$to-list

    #:f2$center #:f2$circ #:f2$polygon #:f2$rect #:f2$square*

    #:f2in-triangle #:f2in-bbox #:f2in-concave
    #:f2segdst #:f2lsegx #:f2segx #:f3planex

    #:vprod #:vsum))

