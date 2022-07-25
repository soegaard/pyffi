#lang racket/base
(require "../python-environment.rkt")
(require (for-syntax racket/base syntax/parse racket/syntax))

(require "../python.rkt")
(set-environment-variables)
(initialize)                
(finish-initialization)

(let ([l (pylist-new 2)])
  (pylist-size l))

(let ([l (pylist-new 2)])
  (pylist-set-item! l 0 "foo")
  (pylist-set-item! l 1 "bar")
  ; (pylist-set-item! l 2 "baz")
  l)

(let ([l (pylist-new 2)])
  (pylist-set-item! l 0 "foo")
  (pylist-set-item! l 1 "bar")
  (equal? (list (pylist-get-item l 0) (pylist-get-item l 1))
          '("foo" "bar")))

(list->pylist   '(1 2 3 "foo"))
(vector->pylist #(1 2 3 "foo"))


(pylist-get-slice  (vector->pylist #(10 11 12 13 14 15 16 17 18 19)) 2 5)
(let ([l (vector->pylist #(10 11 12 13 14 15 16 17 18 19))])
  (pylist-set-slice! l 2 5 (pylist 2 3 4))
  l)

(for/list ([x (in-pylist (vector->pylist #(10 11 12)))])
  x)


(let ([l (pylist-new 0)])
  (pylist-append-item! l "a")
  (pylist-append-item! l "b")
  (pylist-append-item! l "c")
  l)

(let ([l (pylist-new 0)])
  (pylist-insert! l 0 "a")
  (pylist-insert! l 0 "b")
  (pylist-insert! l 0 "c")
  l)

(let ([l (list->pylist (list 1 2 3))])
  (pylist-reverse! l)
  l)

(let ([l (list->pylist (list 1 3 2))])
  (pylist-sort! l)
  l)

(pylist->tuple (pylist 1 2 3))

