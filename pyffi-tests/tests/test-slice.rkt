#lang racket/base
(require "../python.rkt")

(set-environment-variables)
(initialize)                
(import-numpy)
(finish-initialization)


(slice 1)
(displayln (slice 1))
(write (slice 1)) (newline)

(slice-indices (slice 1 2 3) 10)

