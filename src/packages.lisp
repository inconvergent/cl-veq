
(defpackage #:veq
  (:use #:common-lisp)
  (:export
    #:*eps*
    #:dpi #:dpi5 #:dpii #:dpi25
    #:fpi #:fpi5 #:fpii #:fpi25
    #:mac #:mac* #:v? #:d? #:i? #:context? #:ext-symbols?

    #:type-from-short #:arrtype #:type-default
    #:unpack-vvsym #:vvsym

    #:from-lst #:lst
    #:mvb #:dsb
    #:vnrep #:vnval #:vchain
    #:mvc #:mvcwrap #:mvcgrp #:mvcmap #:~

    #:df #:df* #:ff #:ff* #:pos-df #:pos-ff
    #:in #:in* #:kv #:kv*
    #:ll #:ll* #:pn #:pn*
    #:sy #:sy*
    #:dvec #:fvec #:ivec #:kvec #:lvec #:pvec #:svec

    #:ffl #:dfl #:vsel #:xlet #:vv

    #:replace-varg
    #:vdef #:vdef* #:def* #:fvdef* #:fvdef #:vlabels
    #:vprogn #:fvprogn #:varg #:vref

    #:new-stride #:$make #:$copy

    #:$nvset
    #:f2$center #:f2$circ #:f2$polygon #:f2$rect #:f2$square*
    #:$to-list #:2$to-list #:3$to-list #:4$to-list
    #:vp #:vpr #:$print #:2$print #:3$print #:4$print

    #:f2in-triangle #:f2in-bbox #:f2in-concave
    #:f2segdst #:f2lsegx #:f2segx #:f3planex))

