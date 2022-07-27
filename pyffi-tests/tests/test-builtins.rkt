#lang racket/base 
(require pyffi/structs
         pyffi/python-c-api
         pyffi/python-initialization
         pyffi/python-environment
         pyffi/python-evaluation
         pyffi/python-types
         pyffi/python-constants
         pyffi/python-operators
         pyffi/python-builtins
         pyffi/python-list
         pyffi/python-string
         racket/format
         )

(require (for-syntax racket/base
                     syntax/parse))

;;; Start Python and load "numpy"

'before-initialize
(initialize)                 ; handles `main` and `builtins`
'initialize-done
(finish-initialization)      ; run delayed setters

;(void (run* "import operator as operator"))

aiter
(equal? (all '(#t #t #t)) #t)             
(equal? (all '(#t #f #t)) #f)            
(equal? (any '(#f #f #f)) #f)             
(equal? (any '(#f #f #t)) #t)
(equal? (ascii "foo\nbar") "'foo\\nbar'")
(equal? (bin 11) "0b1011")
(equal? (bool 1)  #t)
(equal? (bool #t) #t)
(equal? (bool #f) #f)
(equal? (repr (bytearray 3)) "bytearray(b'\\x00\\x00\\x00')")
(equal? (repr (bytearray "foo" "utf-8")) "bytearray(b'foo')")
(equal? (repr (bytes "foo" "utf-8")) "b'foo'")
(equal? (callable 42) #f)
(equal? (callable (obj "fun" (get 'builtins.any))) #t)
(equal? (chr 97) "a")
(equal? (repr (complex))     "0j")
(equal? (repr (complex 1))   "(1+0j)")
(equal? (repr (complex 1 2)) "(1+2j)")
; (delattr obj "foo") ; todo
(equal? (repr (dict (hash "foo" 42 "bar" 43))) "{'foo': 42, 'bar': 43}")
(equal? (getitem (dict (hash "foo" 42 "bar" 43)) "foo") 42)
(equal? (divmod 11 4) #(2 3))
(equal? (repr (builtins.list (enumerate "foo"))) "[(0, 'f'), (1, 'o'), (2, 'o')]")
; (builtins.eval "1") ; todo
;(builtins.filter (λ (x) #t) "foo")
(equal? (builtins.float "12.34") 12.34)
(equal? (builtins.float)         0.0)
; (equal? (builtins.format #(3 5) "X: {0[0]};  Y: {0[1]}") "X: 3;  Y: 5")
(equal? (builtins.format 17 "x") "11")  ; x means hex format
(equal? (getattr (dict (hash "foo" 42)) "foo" "missing") "missing")
; (builtins.globals) ; todo?
(equal? (hasattr (dict (hash "foo" 42)) "bar") #f)

; 'Problem-large-ints ; Is the problem py-int->integer ?
; try hashing a string 
(builtins.hash 42.3)
(equal? (builtins.hash 42.3) 691752902764101674)

(equal? (hex 17) "0x11")
(let ([d (dict (hash))]) (equal? (builtins.id d) (builtins.id d)))
(equal? (builtins.int) 0)
(equal? (builtins.int "12") 12)
; isinstance
; issubclass
(equal? (let ([i (iter "foo")])
          (map pystring->string (pylist->list (builtins.list i))))
        '("f" "o" "o"))
(equal? (len (builtins.list "foo")) 3)
; (builtins.locals) ; todo
(equal? (let ([i (iter "foo")]) (list (next i) (next i) (next i) (next i "done")))
        '("f" "o" "o" "done"))
(equal? (repr object) "<class 'object'>")
(equal? (ord #\a) 97)
(equal? (builtins.pow 2 3) 8)
(equal? (builtins.pow 2 4 5) 1)  ; 2⁴ mod 5 = 16 mod 5 = 1
(equal? (pylist->list (builtins.list (builtins.range 3))) '(0 1 2))
(equal? (pylist->list (builtins.list (reversed (builtins.range 3)))) '(2 1 0))
(equal? (builtins.round 2.675 2) 2.67)
(equal? (builtins.round 0.5) 0)
(equal? (sorted (builtins.list (builtins.set "foo"))) '("f" "o"))
(equal? (sorted "foo")  '("f" "o" "o"))
(equal? (str 42) "42")
(equal? (str (dict (hash "foo" 42 "bar" 43))) "{'foo': 42, 'bar': 43}")
(equal? (sum '(1 2 3)) 6)
(equal? (builtins.tuple "foo") '#("f" "o" "o"))

;(repr (dir (dict (hash "foo" 42 "bar" 43))))
;; (repr 42)
;; len
;; builtins.slice

