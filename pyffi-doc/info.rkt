#lang info

(define collection 'multi)

(define deps '("scribble-lib"
               "pyffi-lib"
               "base"))

(define build-deps '("sandbox-lib"
                     "gui-doc"
                     "pict-doc"
                     "at-exp-lib"
                     "base"
                     "pict-lib"
                     "scribble-lib"
                     "racket-doc"
                     "pyffi-lib"
                     "rackunit-lib"))

(define update-implies '("pyffi-lib"))

(define pkg-desc "Use Python from Racket - Documentation of \"pyffi\"")

(define pkg-authors '(soegaard))
