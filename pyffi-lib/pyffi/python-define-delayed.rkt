#lang racket/base
(require "python-delayed.rkt"
         "python-environment.rkt")

(require (for-syntax racket/base
                     racket/format
                     racket/match
                     racket/syntax                     
                     syntax/parse))

;;;
;;; SYNTAX (define/delay racket-id qualified-name expr ...)
;;;

; If `qualified-name` is bound in the Python environment,
; evaluate the expressions `expr ...` and bind the result to `racket-id`.
; If `qualified-name` is unbound in the Python environment,
; then bind `racket-id` to `'<uninitialized>` and add a thunk (to the list
; of delayed initialization thunks) that when called evaluates the
; expressions `expr ...` and stores the result in `racket-id`.

; Typical usage: Extract a value/function from the Python environment,
;                if the value is ready. Otherwise, store a think that
;                can extract the value, when it is ready.

(provide define/delay)
(define-syntax (define/delay stx)
  (syntax-parse stx
    [(_define/delay racket-id:id qualified-name:id expr:expr ...)
     (syntax/loc stx
       (begin
         (provide racket-id)
         (define racket-id '<uninitialized>)
         (define (init-thunk)
           (set! racket-id (let () expr ...)))
         (if (id-bound? 'qualified-name)
             (init-thunk)
             (add-initialization-thunk init-thunk))))]))

;;;
;;; SYNTAX (define-delayed
;;;           (define racket-id expr)
;;;           ...)
;;;

;; Wrap a series of `(define racket-id expr)` in `(define-delayed ...)`
;; in order to turn them into `(define/delay racket-id racket-id expr)`.

(provide define-delayed)
(define-syntax (define-delayed stx)
  ; When the racket-id and the qualified-name are the same,
  ; we can wrap a sequence of definitions into `define-delayed`.
  (syntax-parse stx
    [(_define-delayed
      (define racket-id:id expr:expr)
      ...)
     (syntax/loc stx
       (begin
         (provide racket-id ...)
         (define/delay racket-id racket-id expr) ...))]))
