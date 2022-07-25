#lang racket/base
(require (for-syntax racket/base syntax/parse racket/require-transform))

(require
 pyffi/structs
 pyffi/python-c-api
 pyffi/python-initialization
 pyffi/python-environment
 pyffi/python-evaluation
 pyffi/python-types
 pyffi/python-builtins
 pyffi/python-operators
 pyffi/python-attributes
 pyffi/python-functions
 racket/format
 racket/list
 racket/match)



;;;
;;; Start Python and import "numpy"
;;;

(set-environment-variables)
(initialize)                 ; handles `main` and `builtins`
;(import-inspect)
;(require "numpy.rkt")
;(initialize-numpy)           ; load the `numpy` module
;                            ; import and initialize numpy before
;                            ; running the delayed initializers
(finish-initialization)      ; run delayed setters


(define-syntax (define-predicates stx)
  (syntax-case stx ()
    [(_ id ...)
     (syntax/loc stx
       (begin
         (define-py id (~fun ~py -> ~py))
         ...))]))

(define-predicates
  ; inspect.ismodule
  inspect.isclass
  inspect.ismethod
  inspect.isfunction
  inspect.isgeneratorfunction
  inspect.isgenerator
  inspect.iscoroutinefunction
  inspect.iscoroutine
  inspect.isawaitable
  inspect.isasyncgenfunction
  inspect.isasyncgen
  inspect.istraceback
  inspect.isframe
  inspect.iscode
  inspect.isbuiltin
  inspect.isroutine
  inspect.isabstract
  inspect.ismethoddescriptor
  inspect.isgetsetdescriptor
  inspect.ismemberdescriptor)

(define-syntax (define-getters stx)
  (syntax-case stx ()
    [(_ id ...)
     (syntax/loc stx
       (begin
         (define-py id (~fun ~py -> ~py))
         ...))]))

(define-getters
  inspect.getdoc
  inspect.getcomments
  inspect.getfile
  inspect.getmodule
  inspect.getsourcefile
  inspect.getsourcelines
  inspect.getsource)

(define-py inspect.cleandoc  (~fun ~py -> ~py))
;; (define-py inspect.signature (~fun ~py -> ~py))
;; (finish-initialization)

(finish-initialization)
(void (inspect.getmembers               (obj "pyfun" (get 'inspect.getmembers))))
(void (inspect.cleandoc (inspect.getdoc (obj "pyfun" (get 'inspect.getmembers)))))


;; (~pyprocedure
;;       input-types    ; list of pytype
;;       output-type
;;       keywords       ; #f or list of symbols
;;       keyword-types  ; #f or list of ptype
;;       optional-after ; if non-#f, the index of the first optional argument
;;       )



; (struct pyproc (positional-parameters positional-excess keyword-parameters keyword-excess))
; (struct pyprocedure (input-types output-type keywords keyword-types optional-after) #:transparent)

;; (struct pyproc (positional-parameters positional-types positional-excess 
;;                 keyword-parameters    keyword-types    keyword-excess
;;                 first-optional result-type)
;;   #:transparent)


 

;; (define p (get-fun 'inspect.ismodule))
;; (displayln (list 'HERE p))
;; (define-py inspect.ismodule p)
;; (define-py builtins.repr p)
;; (finish-initialization)

;; (builtins.repr 42)
; (inspect.ismodule 42)



;; (define transpose (pyproc/keywords (get-fun-as-pyproc 'numpy.transpose)))
;; ;(define array      (pyproc/keywords (get-fun-as-pyproc 'numpy.array)))

;; array
;; (array '[[1 2] [3 4]])
;(transpose )

;(get-fun 'inspect.signature)

;  (struct pyprocedure (input-types output-type keywords keyword-types optional-after))
  
  #;(for/list ([i (len ps)]) (getitem ps i))
  #;(list (.empty s)
        (len )
        (.return_annotation s))

; inspect.signature(inspect.signature).parameters.values()
; inspect.signature(inspect.signature).parameters.items()




; >>> import inspect
; >>> inspect.signature(inspect.signature)

; <Signature (obj, *, follow_wrapped=True, globals=None, locals=None, eval_str=False)>

;; POSITIONAL_ONLY
;; Value must be supplied as a positional argument. Positional only
;; parameters are those which appear before a / entry (if present) in a
;; Python function definition.

;; POSITIONAL_OR_KEYWORD
;; Value may be supplied as either a keyword or positional argument (this
;; is the standard binding behaviour for functions implemented in
;; Python.)

;; VAR_POSITIONAL
;; A tuple of positional arguments that aren’t bound to any other
;; parameter. This corresponds to a *args parameter in a Python function
;; definition.

;; KEYWORD_ONLY
;; Value must be supplied as a keyword argument. Keyword only parameters
;; are those which appear after a * or *args entry in a Python function
;; definition.

;; VAR_KEYWORD
;; A dict of keyword arguments that aren’t bound to any other
;; parameter. This corresponds to a **kwargs parameter in a Python
;; function definition.


;; (empty #(struct:obj "type" #<cpointer>)
;;        name "obj"
;;        default Empty
;;        annotation #(struct:obj "type" #<cpointer>)
;;        kind #(struct:obj "_ParameterKind" #<cpointer>)
;;        kind.description "positional or keyword")

;; (empty #(struct:obj "type" #<cpointer>)
;;        name "follow_wrapped"
;;        default #t
;;        annotation #(struct:obj "type" #<cpointer>)
;;        kind #(struct:obj "_ParameterKind" #<cpointer>)
;;        kind.description "keyword-only")

;; (empty #(struct:obj "type" #<cpointer>)
;;        name "globals"
;;        default None
;;        annotation #(struct:obj "type" #<cpointer>)
;;        kind #(struct:obj "_ParameterKind" #<cpointer>)
;;        kind.description "keyword-only")

;; (empty #(struct:obj "type" #<cpointer>)
;;        name "locals"
;;        default None
;;        annotation #(struct:obj "type" #<cpointer>)
;;        kind #(struct:obj "_ParameterKind" #<cpointer>)
;;        kind.description "keyword-only")

;; (empty #(struct:obj "type" #<cpointer>)
;;        name "eval_str"
;;        default #f
;;        annotation #(struct:obj "type" #<cpointer>)
;;        kind #(struct:obj "_ParameterKind" #<cpointer>)
;;        kind.description "keyword-only")

