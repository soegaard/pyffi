#lang racket/base
(provide initialize-builtin-constants
         True False None)

(require "python-evaluation.rkt")

(define True  #f)
(define False #f)
(define None  #f)

(define (initialize-builtin-constants)
  (set! True  (run "True")) ; Note: These can't be returned directly
  (set! False (run "False")) ;       Due to reference counting, 
  (set! None  (run "None")))
  
