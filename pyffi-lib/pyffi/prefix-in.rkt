#lang racket/base
(require (for-syntax racket/base syntax/parse racket/require-transform))

(provide prefix-in)

(define-syntax prefix-in
  (make-require-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ rel-path:string in:string ...)
        (define rp     (syntax->datum #'rel-path))
        (define ins    (syntax->datum #'(in ...)))
        (define rp/ins (for/list ([i ins]) (string-append rp "/" i)))
        (with-syntax ([(rp/in ...) (datum->syntax stx rp/ins)])
          (expand-import #`(combine-in rp/in ...)))]))))
