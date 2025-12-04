#lang racket/base
(provide current-repr
         current-str
         current-error-on-not-found?
         current-pygenerator-prop:sequence)

(define current-repr (make-parameter (λ (x) "current-repr not set to `repr` yet")))
(define current-str  (make-parameter (λ (x) "current-str not set to `str` yet")))

(define current-error-on-not-found? (make-parameter #t))

(define current-pygenerator-prop:sequence
  ; used for prop:sequence of generator-obj structures, see "python-generator.rkt"
  (make-parameter (λ (x) "current-pygenerator-prop:sequence not set yet")))

