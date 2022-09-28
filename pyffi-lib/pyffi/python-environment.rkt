#lang racket/base

(provide
 import-into-python
 initialize-main-and-builtins
 import-numpy
 
 main builtins globals locals get get*
 get-module
 id-bound?)

(require "structs.rkt"
         "python-c-api.rkt"
         ; (only-in "python-builtins.rkt" getattr)
         racket/string racket/match racket/format)


; (define-py getattr (~fun ~py ~py ~py -> ~py #:first-optional 2) #:from builtins)


(define main      #f)
(define builtins  #f)
(define globals   #f)
(define locals    #f)

(define mod:operator  #f)
;(define mod:traceback #f)

(define (initialize-main-and-builtins)
  ; (displayln (list 'initialize-main-and-builtins))
  ; AddModule doesn't load or impor the module, it just returns it.
  (set! main          (PyImport_AddModule "__main__"))     ; top-level environment
  (set! builtins      (PyImport_AddModule "builtins"))
  (PyImport_AddModule "operator")
  ; (PyImport_AddModule "traceback")

  (set! globals  (PyModule_GetDict main))             ; globally defined variables
  (set! locals   (PyDict_New))
  
  (define empty-from-list (PyList_New 0))
  ; Add `builtins` and `operators` to the global symbol table
  ; (PyImport_ImportModuleEx "builtins"  globals globals empty-from-list)
  (PyModule_AddObjectRef main "__builtins__" builtins) ; use the name "builtins" for the value builtins.
  (PyModule_AddObjectRef main "builtins"     builtins) ; use the name "builtins" for the value builtins.
  
  (set! mod:operator
        (PyImport_ImportModuleEx "operator" globals globals empty-from-list))
  (void (PyModule_AddObjectRef main "operator" mod:operator))

  ; Removed temporatily
  #;(import-into-python 'traceback)
  #;(import-into-python 'inspect)

  (void))

(define (import-into-python module-sym [as #f])
  ; import `module-sym` into the Python environment
  (define module-str      (~a module-sym))
  (define empty-from-list (PyList_New 0))
  (define mod             (PyImport_ImportModuleEx module-str globals globals empty-from-list))
  ; (displayln (list 'import-into-python (or (and as (~a as)) module-str)))
  (cond
    [mod  (void (PyModule_AddObjectRef main (or (and as (~a as)) module-str) mod))]
    [else (error 'import (~a "No Python module named '" module-sym "'"))]))

(define (import-numpy)
  (define empty-from-list (PyList_New 0))
  (define mod:numpy (PyImport_ImportModuleEx "numpy" globals globals empty-from-list))
  (when mod:numpy
    (void (PyModule_AddObjectRef main "numpy" mod:numpy)))) ; 0=success


(define (id-bound? sym [dict globals])
  (and main
       (case sym
         [(main)  #t]
         ; [(builtins) (and builtins #t)]
         [else  (and dict
                     (PyDict_GetItemString dict (~a sym))
                     #t)])))


(define (get sym [type #f])
  ;; (newline) (displayln "--") (newline)
  ;; (displayln (list 'get 'sym: sym))
  ;; (displayln (list 'main: main 'globals: globals))
  ;; (displayln (list 'get sym type))
  (define names (string-split (symbol->string sym) "."))
  (match names
    [(list* "builtins" names*)
     ;(displayln "using builtins")
     ;(displayln (PyUnicode_AsUTF8 (PyObject_Str builtins)))
     ;(displayln (PyDict_GetItemString builtins "len"))
     (get* builtins names* names #f sym type)]
    [(list* "main" names*)
     (get* main names* names #f sym type)]
    [_
     (get* globals  names  names #f sym type)]))


          
(define (get* obj names all-names prefix orig-sym type)
  ; (begin (write (list 'get* obj names all-names orig-sym type)) (newline))
  #;(displayln (PyUnicode_AsUTF8 (PyObject_Str obj)))
  ; obj is a either a module or a dict
  (match names
    [(list)
     (unless obj (error 'get (~a "not bound xx: " orig-sym)))
     obj]
    [(list* name names) ; reference to a global variable
     (define typename (python-type obj))
     (define item
       (case typename
         ; Note: Use `getattr` here. If a package has "automodules" the
         ;       dynamically generated modules don't show up in the dict
         ;       associated with the module.
         ; Note: Benchmark: Would a fast path with dict and a fallback
         ;       to `getattr` be faster?
         [("module" "_automodule") (PyObject_GetAttrString obj name)]
         [("dict")                 (PyDict_GetItemString obj name)]
         [("type")                 (PyObject_GetAttrString obj name)]
         [else
          (error 'get* (~a "got object of type : " typename))]))     
     (unless item
       (error 'get (~a "not bound: yy " name " in " orig-sym)))
     (define new-prefix (if prefix (string-append prefix name ".") (string-append name ".")))
     (get* item names all-names new-prefix orig-sym type)]))


(define (get-module sym)
  (define mod (get sym))
  (obj "module" mod))




