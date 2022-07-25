#lang racket

(define foo
  (let ()
    (define proc (λ () (raise (exn "hello" (current-continuation-marks)))))
    (procedure-rename proc 'bar)))

(displayln (foo))

