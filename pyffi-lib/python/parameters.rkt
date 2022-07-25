#lang racket/base
(provide current-repr
         current-str)

(define current-repr (make-parameter (λ (x) "current-repr not set to `repr` yet")))
(define current-str  (make-parameter (λ (x) "current-str not set to `str` yet")))
   
