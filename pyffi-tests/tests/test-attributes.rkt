#lang racket

(require pyffi)

(initialize)
(post-initialize)

main
main.builtins
main.builtins.list
(main.builtins.list "abc")

;(pretty-print (syntax->datum (expand #'builtins.list)))
;(pretty-print (syntax->datum (expand #'main.builtins.list)))

builtins
builtins.list
(builtins.list "abc")
(builtins.dir (builtins.list "abc"))
((builtins.list "abc").count "a")

((builtins.list "abc").copy .count "a") ; todo: make this work

;(len (builtins.list "abc")) ; note: no `len` method


;(hasattr main "list")
;(builtins.dir main)
;(getattr main "list")

