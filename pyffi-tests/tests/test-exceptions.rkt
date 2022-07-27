#lang racket

(require pyffi)
(initialize)                
(finish-initialization)

(void (run* "def provoke0(baz=1): return 2/0"))
(define provoke0 (get-fun 'provoke0))
(provoke0)



