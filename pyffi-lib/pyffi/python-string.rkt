#lang racket/base

(provide (all-defined-out))

(require "structs.rkt"
         "python-c-api.rkt"
         "python-environment.rkt"
         "python-operators.rkt"
         "python-slice.rkt"
         "python-types.rkt")

(require (for-syntax racket/base syntax/parse racket/syntax))

;;;
;;; Strings (Unicode Objects)
;;;

(define (pystring? x)
  (and (obj? x) (equal? (obj-type-name x) "str")))

(define (string->pystring x)
  (unless (string? x) (error 'string->pystring "got: ~a" x))
  (obj "str" (PyUnicode_FromString x)))

(define (pystring->string x)
  (unless (pystring? x) (error 'pystring->string "got: ~a" x))
  (PyUnicode_AsUTF8 (obj-the-obj x)))


(define (pystring-length x)
  (unless (pystring? x) (error 'pystring-length "got: ~a" x))
  (PyObject_Length (obj-the-obj x)))

(define (pystring-ref x i)
  (unless (pystring? x)
    (error 'pystring-length "got: ~a" x))
  (unless (<= 0 i (pystring-length x))
    (error 'pystring-length "index ~a out of range for the string '~a: " i x))

  ; Todo: should this return a Racket character instead?
  (string-ref (PyUnicode_AsUTF8 (PyObject_GetItem (obj-the-obj x) (PyLong_FromLong i))) 0))

(define (subpystring x start [end #f])
  (define who 'subpystring)
  (unless (pystring? x) (error who "got: ~a" x))
  ; (getitem x (slice start end)) ; alternative
  (obj "str" (PyUnicode_Substring (obj-the-obj x) start (or end -1))))

#;(define (pystring-slice x slice)
  (define who 'pystring-slice)
  (unless (pystring? x) (error pystring-slice "got: ~a" x))
  (getitem x slice))

(define pystring-slice 
  (case-lambda
    [(x stop)            (getitem x (slice stop))]
    [(x start stop)      (getitem x (slice start stop))]
    [(x start stop step) (getitem x (slice start stop step))]))

(define (in-pystring pystring)
  (let ([s (pystring->string pystring)])
    (in-string s)))
