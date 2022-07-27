#lang racket/base
(require pyffi)

(initialize)                
(import-numpy)
(finish-initialization)

(string->pystring "foo")
(pystring->string (string->pystring "foo"))

(pystring-length (string->pystring "foo"))

(pystring-ref (string->pystring "foo") 0)
(pystring-ref (string->pystring "foo") 1)
(pystring-ref (string->pystring "foo") 2)

(subpystring (string->pystring "foobarbaz") 3 6)

(pystring-slice (string->pystring "0123456789abcedf") 2 10 2)

(for/list ([x (in-pystring (string->pystring "foo"))]) x)



