#lang racket/base
(require pyffi)
(require (for-syntax racket/base syntax/parse racket/syntax))

(initialize)                
(finish-initialization)


(let ([d (dict-new)])
  (dict-set-item! d "foo" "f")
  (dict-set-item! d "bar" 42)
  (dict-del-item! d "foo")
  d
  #;(list (dict-keys d) (dict-values d))
  #;(equal? (list (dict-keys d) (dict-values d))
          (list '("bar") '(42))))


(let ([d (dict-new)])
  (dict-set-item! d "foo" "f")
  (dict-set-item! d "bar" 42)
  (list (dict-get-item d "bar")
        (dict-get-item d "foo")
        (dict-get-item d "baz")))

(let ([d1 (dict-new)] [d2 (dict-new)])
  (dict-set-item! d1 "foo" "f")
  (dict-set-item! d1 "bar" 42)
  (dict-set-item! d2 "foo" "g")
  (dict-set-item! d2 "baz" 43)
  (dict-merge! d1 d2)
  d1)

