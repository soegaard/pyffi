#lang info

(define collection 'multi)

(define deps       '("pyffi-doc" "pyffi-lib"))
(define build-deps '("pyffi-doc" )) ; omittable from a binary package
(define implies    '("pyffi-lib"))

(define pkg-desc "Use Python from Racket.")

(define pkg-authors '(soegaard))

(define test-responsibles '((all jensaxel@soegaard.net)))
