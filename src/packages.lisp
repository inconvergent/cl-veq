
#+:veq-simd (defpackage #:avx (:use #:sb-simd-avx2))

(defpackage #:veq
  (:use #:common-lisp)
  (:export #:*eps*
           #:dpi #:dpi5 #:dpii #:dpi25
           #:fpi #:fpi5 #:fpii #:fpi25
    #:unpack-vvsym #:vvsym
    #:mac #:mac* #:v? #:d? #:i? #:context? #:ext-symbols?
    #:group #:ungroup #:strip-arg-keys #:get-arg-key
    #:type-from-short #:arrtype #:type-default
    #:make-struct-vec-getters #:make-struct-vec-setters

    #:df #:ff #:in #:pn #:kv #:sy #:ll #:pos-df #:pos-ff
    #:dvec #:fvec #:ivec #:kvec #:lvec #:pvec #:svec
    #:2dvec #:2fvec #:3dvec #:3fvec #:4dvec #:4fvec

    #:falpha #:dalpha #:feps= #:deps=

    #:~ #:mvb #:dsb #:xlet #:from-lst #:lst
    #:vnrep #:vnval #:vchain #:mutate! #:with-symbs #:vector-rearrange #:lpos
    #:mvc #:mvcwrap #:mvcgrp #:mvcmap  #:vsel

    #:vdef #:vdef* #:def* #:fvdef* #:fvdef #:vlabels
    #:vv #:vvdb #:proc-vv #:replace-varg
    #:vprogn #:fvprogn #:varg #:vref

    #:new-stride #:$make #:$copy #:$nvset #:$rowset
    #:f2$center #:f2$circ #:f2$polygon #:f2$rect #:f2$square*
    #:$to-list #:2$to-list #:3$to-list #:4$to-list
    #:vp #:vpr #:$print #:2$print #:3$print #:4$print

    #:f2in-triangle #:f2in-bbox #:f2in-concave
    #:f2lsegx #:f2segx #:f2ssegx #:f2segdst #:f3planex

    #:fmake-view-matrix #:fmake-proj-matrix #:fmake-ortho-proj-matrix))

