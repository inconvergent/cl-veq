
#+:veq-simd (defpackage #:avx (:use #:sb-simd-avx2))

(defpackage #:veq
  (:use #:common-lisp)
  (:export
    #:*eps*
    #:dpi #:dpi5 #:dpii #:dpi25
    #:fpi #:fpi5 #:fpii #:fpi25
    #:mac #:mac* #:v? #:d? #:i? #:context? #:ext-symbols?
    #:group #:ungroup #:strip-arg-keys #:get-arg-key

    #:df #:df* #:ff #:ff* #:pos-df #:pos-ff
    #:ffl #:dfl
    #:in #:in* #:kv #:kv*
    #:ll #:ll* #:pn #:pn*
    #:sy #:sy*
    #:dvec #:fvec #:ivec #:kvec #:lvec #:pvec #:svec
    #:vsel

    #:type-from-short #:arrtype #:type-default
    #:unpack-vvsym #:vvsym

    #:from-lst #:lst
    #:mvb #:dsb #:xlet
    #:vnrep #:vnval #:vchain #:mutate! #:with-symbs #:vector-rearrange #:lpos
    #:mvc #:mvcwrap #:mvcgrp #:mvcmap #:~

    #:vdef #:vdef* #:def* #:fvdef* #:fvdef #:vlabels
    #:vprogn #:fvprogn #:varg #:vref
    #:vv #:proc-vv #:replace-varg

    #:new-stride #:$make #:$copy

    #:$nvset #:$rowset
    #:f2$center #:f2$circ #:f2$polygon #:f2$rect #:f2$square*
    #:$to-list #:2$to-list #:3$to-list #:4$to-list
    #:vp #:vpr #:$print #:2$print #:3$print #:4$print

    #:f2in-triangle #:f2in-bbox #:f2in-concave
    #:f2segdst #:f2lsegx #:f2segx #:f3planex))


