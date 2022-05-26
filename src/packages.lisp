
(defpackage #:veq
  (:use #:common-lisp)
  (:export
    #:*eps* #:dpi #:dpi5 #:dpii #:fpi #:fpi5 #:fpii

    #:mac #:mac*
    #:v? #:d? #:i? #:context?  #:ext-symbols?

    #:ffl #:dfl #:~
    #:from-lst #:lst #:mvb #:mvc #:mvcwrap #:dsb #:vgrp-mvc

    #:df #:df* #:dvec #:ff #:ff* #:fvec #:in #:in* #:ivec

    #:vdef #:vdef* #:def* #:fvdef* #:fvdef #:vlabels
    #:vprogn #:fvprogn #:varg #:vref

    #:d2let #:d3let #:d4let #:f2let #:f3let #:f4let

    #:d$_ #:d_ #:f$_ #:f_
    #:$ #:2$ #:3$ #:4$

    #:$print #:2$print #:3$print #:4$print #:vpr
    #:$num #:2$num #:3$num
    #:$to-list #:2$to-list #:3$to-list #:4$to-list

    #:d$line #:d$point #:d2$line #:d2$point
    #:d3$line #:d3$point #:d4$line #:d4$point

    #:f$line #:f$point #:f2$line #:f2$point
    #:f3$line #:f3$point #:f4$line #:f4$point

    #:f2$center #:f2$circ #:f2$polygon #:f2$rect #:f2$square

    #:f2in-triangle #:f2in-bbox #:f2in-concave
    #:f2segdst #:f2lsegx #:f2segx
    #:f3planex

    #:f$ #:d$ #:f2$ #:d2$ #:f3$ #:d3$ #:f4$ #:d4$

    #:vprod #:vsum))

