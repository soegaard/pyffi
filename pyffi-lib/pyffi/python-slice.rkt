#lang racket/base

(provide (all-defined-out))

(require "structs.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-types.rkt")

(require (for-syntax racket/base syntax/parse racket/syntax))

;;;
;;; Slice Objects
;;;

(define (slice? x)
  (and (obj? x) (equal? (obj-type-name x) "slice")))

(define slice 
  (let ([c integer->py-int])
    (case-lambda
      [(stop)            (obj "slice" (PySlice_New #f (c stop) #f))]
      [(start stop)      (if start
                             (if stop
                                 (obj "slice" (PySlice_New (c start) (c stop) #f))
                                 (obj "slice" (PySlice_New (c start)     #f   #f)))
                             (obj "slice" (PySlice_New #f (c stop) #f)))]
      [(start stop step) (obj "slice" (PySlice_New (c start) (c stop) (c step)))])))

(define (slice-indices x length)
  (unless (slice? x) (error 'slice-indices "expected a slice, got: ~a" x))
  (PySlice_GetIndices (obj-the-obj x) length))





