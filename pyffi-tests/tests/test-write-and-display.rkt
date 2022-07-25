#lang racket
(require pyffi)

(define hw (string->pystring "Hello World"))

(write   hw) (newline)
(display hw) (newline)
