#lang racket

(require pyffi)

(initialize)
(post-initialize)

(define x (PyBool_FromLong 0))
(collect-garbage)
(set! x #f)
(collect-garbage)

'foo
(define xs (pylist))
(collect-garbage)
(set! xs #f)
(collect-garbage)
(sleep 10)


