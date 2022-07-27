#lang racket/base
(require pyffi/structs
         pyffi/python-c-api
         pyffi/python-initialization
         pyffi/python-environment
         pyffi/python-evaluation
         pyffi/python-types
         ;"python-constants
         pyffi/python-builtins
         pyffi/python-operators
         pyffi/python-attributes
         pyffi/python-functions
         racket/format
         racket/list
         racket/match)

(require (for-syntax racket/base syntax/parse))


;;;
;;; Start Python and import "numpy"
;;;

(initialize)                 ; handles `main` and `builtins`
;(import-inspect)
;(require "numpy.rkt")
;(initialize-numpy)           ; load the `numpy` module
;                            ; import and initialize numpy before
;                            ; running the delayed initializers
(finish-initialization)      ; run delayed setters


'h1
((get-fun 'inspect.ismodule)
 42)

'h2
((get-fun 'operator.add)
 11 22)

; Test only positional arguments from 0,1,2 to many
'foo0
(void (run* "def foo0(): return 1"))
((get-fun 'foo0))

'foo1
(void (run* "def foo1(x): return x+10"))
((get-fun 'foo1)
 2)

'foo2
(void (run* "def foo2(x,y): return x+y"))
((get-fun 'foo2)
 11 33)

'foo3
(void (run* "def foo3(x,y,z): return x+y+z"))
((get-fun 'foo3)
 100 20 3)

;; Test positional excess
'bar0
(void (run* "def bar0(*xs): return sum(xs)"))
((get-fun 'bar0)
 100 20 3)

'bar1
(void (run* "def bar1(x0,*xs): return x0+sum(xs)"))
((get-fun 'bar1)
 100 20 4)

'bar2
(void (run* "def bar2(x0,x1,*xs): return x0+x1+sum(xs)"))
((get-fun 'bar2)
 100 20 5)

'bar3
(void (run* "def bar3(x0,x1,x2,*xs): return x0+x1+x2+sum(xs)"))
((get-fun 'bar3)
 1000 200 30 4)

'baz0
(void (run* "def baz0(k=0): return k"))
(get-fun 'baz0)
((get-fun 'baz0)
 #:k 10)

'baz1
(void (run* "def baz1(a,k=0): return a+k"))
(get-fun 'baz1)
((get-fun 'baz1)
 10 #:k 2)

'baz2
(void (run* "def baz2(a,b,k=0,l=1): return a+b+k+l"))
(get-fun 'baz2)
((get-fun 'baz2)
 1000 200 #:k 30 #:l 4)

'baz3
(void (run* "def baz3(a,b,c,k=0,l=1,m=2): return a+b+c+k+l+m"))
(get-fun 'baz3)
((get-fun 'baz3)
 100000 20000 3000 #:k 400 #:l 50 #:m 6)


(get-fun 'baz3)
