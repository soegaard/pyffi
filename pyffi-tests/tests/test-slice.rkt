#lang racket/base
(require pyffi)

(initialize)                
(import-numpy)
(finish-initialization)


(slice 1)
(displayln (slice 1))
(write (slice 1)) (newline)

(slice-indices (slice 1 2 3) 10)

