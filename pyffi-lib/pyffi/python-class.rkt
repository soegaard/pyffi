#lang racket/base
;;;
;;; Python Classes
;;;

; The class statement produces a class object.

; The type of a class object is `type`.


; Class objects support two kinds of operations: attribute references and instantiation.

; - Attribute references use the standard syntax used for all attribute references in Python: obj.name.
;   That is: we can use `getattr` to implement obj.name where obj is a class object.
; - Class attributes can be assigned to.

; - Class instantiation uses function notation.
;   Any arguments given are passed on to the method `__init__`.
; - Signature:  <Signature (**options)>

; Instance objects support:
;   - attribute reference

; An attribute reference can be a "data attribute reference" or a "method attribute reference".

; By definition, all attributes of a class that are function objects define corresponding methods of its instances.

; Note if `c` is a class and `i` is an instance of c`: then if `c.f` is a function, then `i.f` is a method object.




