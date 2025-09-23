
#+:veq-simd (defpackage #:avx (:use #:sb-simd-avx2))

(defpackage #:veq
  (:use #:common-lisp)
  (:export #:*eps* #:*srndopt* #:*opt*
           #:dpi #:dpi5 #:dpii #:dpi25
           #:fpi #:fpi5 #:fpii #:fpi25
    #:unpack-vvsym #:vvsym
    #:mac #:mac* #:v? #:d? #:i? #:context? #:ext-symbols?
    #:group #:ungroup #:strip-arg-keys #:get-arg-key #:group-into-alist
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

    #:$new-stride #:$make #:$copy #:$nvset #:$rowset
    #:f2$center #:f2$circ #:f2$polygon #:f2$rect #:f2$square*
    #:$to-list #:2$to-list #:3$to-list #:4$to-list
    #:vp #:vpr #:$print #:2$print #:3$print #:4$print

    #:f2in-triangle #:f2in-bbox #:f2in-concave
    #:f2lsegx #:f2segx #:f2ssegx #:f2segdst #:f3planex

    #:fmake-view-matrix #:fmake-proj-matrix #:fmake-ortho-proj-matrix))

(defpackage #:rnd
  (:nicknames #:veq/rnd)
  (:use #:common-lisp)
  (:import-from #:veq #:*opt*)
  (:export
    #:make-rnd-state #:set-rnd-state
    #:2in-circ #:2in-rect #:2in-square #:2nin-circ
    #:2nin-rect #:2nin-square #:2non-circ #:2non-line #:2non-line*
    #:2on-circ #:2on-line #:2on-line*
    #:2walker #:2walker-acc
    #:3in-box #:3in-cube #:3in-sphere
    #:3nin-box #:3nin-cube #:3nin-sphere
    #:3non-line #:3non-line* #:3non-sphere
    #:3on-line #:3on-line* #:3on-sphere
    #:3walker #:3walker-acc
    #:norm #:bernoulli #:2ndistsample #:max-distance-sample
    #:nrnd #:nrnd* #:nrnd-from #:nrnd-from* #:nrndi #:nrndrng #:nrndrngi
    #:prob #:prob* #:rcond #:either
    #:rep #:rnd #:rnd* #:rndget #:shuffle #:make-halton-gen  #:hammersley-pt
    #:rndi #:rndrng #:rndrngi #:array-split
    #:rndspace #:rndspacei
    #:walker #:walker-acc))

(defpackage #:srnd
  (:nicknames #:veq/srnd)
  (:use #:common-lisp)
  (:import-from #:veq #:*srndopt*)
  (:export #:2in-circ #:2on-circ
           #:2in-circ+ #:2on-circ+
           #:3in-sphere+ #:3on-sphere+
           #:3in-sphere #:3on-sphere #:3on-hemi #:3in-hemi #:3diffuse
           #:rndi #:rnd #:rnd* #:rndrng #:make #:srnd #:prob #:rcond))

