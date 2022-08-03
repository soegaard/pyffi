#lang racket/base
(provide (all-defined-out))

(require "structs.rkt")

(require ffi/unsafe)

(define (fast-obj-eq? x y)
  (ptr-equal? (obj-the-obj x) (obj-the-obj y)))

