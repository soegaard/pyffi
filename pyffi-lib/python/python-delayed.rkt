#lang racket/base
(provide run-initialization-thunks
         add-initialization-thunk)

;; The forms `define-delayed` and `define/delay` are in "python-define-delayed.rkt".


(define initialization-thunks '())

(define (add-initialization-thunk thunk)
  (set! initialization-thunks
        (cons thunk initialization-thunks)))

(define (run-initialization-thunks)
  (for ([t (in-list (reverse initialization-thunks))])
    (t))
  (set! initialization-thunks '()))

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))


