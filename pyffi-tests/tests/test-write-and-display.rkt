#lang racket
(require "../python.rkt")

(define hw (string->pystring "Hello World"))

(write   hw) (newline)
(display hw) (newline)
