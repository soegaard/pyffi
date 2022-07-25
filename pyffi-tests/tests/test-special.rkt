#lang racket
(require pyffi/python-attributes)
(provide (all-from-out pyffi/python-attributes))

(test-special foo)
(test-special bar)

(declare-special-prefix foo)

(test-special foo)
(test-special bar)

'here
