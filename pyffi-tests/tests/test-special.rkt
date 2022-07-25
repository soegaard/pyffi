#lang racket
(require "../python-attributes.rkt")
(provide (all-from-out "../python-attributes.rkt"))

(test-special foo)
(test-special bar)

(declare-special-prefix foo)

(test-special foo)
(test-special bar)

'here
