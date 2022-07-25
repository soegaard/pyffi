#lang racket/base

(require "structs.rkt"
         "python-c-api.rkt"
         "python-delayed.rkt"
         "python-define-delayed.rkt"
         "python-environment.rkt"
         "python-evaluation.rkt"
         "python-initialization.rkt"
         
         "python-types.rkt"
         "python-attributes.rkt"
         "python-builtins.rkt"
         "python-functions.rkt"
         "python-operators.rkt"         
         
         racket/format
         racket/list
         racket/match)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))


(define (simple-proc name qualified-name
                     positional-parameters
                     keyword-parameters
                     #:object-type-str   [object-type-str   "fun"]
                     #:positional-excess [positional-excess #f]
                     #:keyword-excess    [keyword-excess    #f]
                     #:positional-types  [positional-types  #f]
                     #:keyword-types     [keyword-types     #f]
                     #:first-optional    [first-optional    #f]
                     #:result-type       [result-type       #f])
  (define object (obj object-type-str (get qualified-name)))
  (set! name (~a name))                                       ; allow symbol
  (set! positional-parameters (map ~a positional-parameters)) ; allow symbols
  (set! keyword-parameters    (map ~a keyword-parameters))    ; allow symbols
  (when positional-excess
    (set! positional-excess (~a positional-excess)))          ; allow symbol
  (when keyword-excess
    (set! keyword-excess (~a keyword-excess)))                ; allow symbol
  (unless positional-types
    (set! positional-types (make-list (length positional-parameters) #f)))
  (unless keyword-types
    (set! keyword-types    (make-list (length keyword-parameters)    #f)))  
  (pyproc object name qualified-name
          positional-parameters positional-types positional-excess 
          keyword-parameters    keyword-types    keyword-excess
          first-optional result-type))

(define (simple-builtin name qualified-name
                        positional-parameters
                        keyword-parameters
                        #:positional-excess [positional-excess #f]
                        #:keyword-excess    [keyword-excess    #f]
                        #:positional-types  [positional-types  #f]
                        #:keyword-types     [keyword-types     #f]
                        #:first-optional    [first-optional    #f]
                        #:result-type       [result-type       #f])
  (pyproc->procedure
   (simple-proc name qualified-name
                positional-parameters
                keyword-parameters
                #:object-type-str   "builtins"
                #:positional-excess positional-excess
                #:keyword-excess    keyword-excess
                #:positional-types  positional-types
                #:keyword-types     keyword-types
                #:first-optional    first-optional
                #:result-type       result-type)))


(define-delayed
  (define builtins.dir
    (simple-builtin 'dir 'builtins.dir
                    '(object) '())))
  #;(define dir
    (pyproc->procedure
     (pyproc (obj "fun" (get 'builtins.dir)) "dir" 'builtins.dir
             '("object") '(#f) #f
             '() #f #f
             #f #f)))
