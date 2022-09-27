#lang racket/base
(require pyffi)
(require (for-syntax racket/base syntax/parse racket/syntax))

(initialize)                
(finish-initialization)


#;(let ([d (pydict-new)])
  (pydict-set!    d "foo" "f")
  (pydict-set!    d "bar" 42)
  (pydict-delete! d "foo")
  d
  #;(list (dict-keys d) (dict-values d))
  #;(equal? (list (dict-keys d) (dict-values d))
          (list '("bar") '(42))))


#;(let ([d (pydict-new)])
  (pydict-set! d "foo" "f")
  (pydict-set! d "bar" 42)
  (list (pydict-ref d "bar")
        (pydict-ref d "foo")
        (pydict-ref d "baz")))

#;(let ([d1 (pydict-new)] [d2 (pydict-new)])
  (pydict-set! d1 "foo" "f")
  (pydict-set! d1 "bar" 42)
  (pydict-set! d2 "foo" "g")
  (pydict-set! d2 "baz" 43)
  (pydict-merge! d1 d2)
  d1)

(pydict->hash (hash->pydict (hash "a" 1  "b" 2)))

(pydict "a" 1 "b" 2)

(pydict->key/values (pydict "a" 1 "b" 2))

(for/list ([(k v) (in-pydict (pydict "a" 1 "b" 2))])
  (vector k v))
