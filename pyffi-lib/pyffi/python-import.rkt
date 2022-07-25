#lang racket/base
(provide import
         import-from
         as)

;;;
;;; IMPORT
;;;

;; This module export the two forms `import` and `import-from`.
;; The intent is that they match the Python statements "import" and "from".

;; SYNTAX
;;      (import dotted-name)
;; or   (import dotted-name as as-name)
;; where  `as` is a keyword.

;; Import `dotted-name` in the Python environment.
;; Get a module object and bind it to either `dotted-name` or `as-name`.


;; The Python docs say the following about the `from` statement:

;; The `from` form uses a slightly more complex process:

;;   1. find the module specified in the from clause, loading and initializing it if necessary;
;;   2. for each of the identifiers specified in the import clauses:
;;      1. check if the imported module has an attribute by that name
;;      2. if not, attempt to import a submodule with that name and then check
;;         the imported module again for that attribute
;;      3. if the attribute is not found, ImportError is raised.
;;      4. otherwise, a reference to that value is stored in the local namespace,
;;         using the name in the as clause if it is present, otherwise using the
;;         attribute name


(require ; "python.rkt"
         ; "python-c-api.rkt"
         "python-environment.rkt"
         "python-functions.rkt"
         "python-module.rkt"
         "python-types.rkt")

(require (for-syntax ; (except-in "python.rkt" bytes #%app)
                     "python-environment.rkt"
                     racket/base
                     racket/string
                     (except-in syntax/parse str)
                     racket/syntax))

(define (get-obj qualified-name)
  (pr (get qualified-name)))

(define-syntax (as stx) (raise-syntax-error 'as "used outside import/import-from form" stx))

(begin-for-syntax
  (define (identifier->string id)
    (cond
      [(string? id)                           id]
      [(symbol? id) (symbol->string           id)]
      [else         (symbol->string (syntax-e id))]))
  
  (define (identifier-split id sep)
    ; Like string-split, but for identifiers.
    ; Returns a syntax object with a list of strings.
    (define str    (identifier->string id))
    (define parts  (string-split str sep))
    (define ctx    id)
    (define srcloc id)
    (define (-> x) (datum->syntax ctx x srcloc))
    (-> (map -> parts)))

  (define (identifier-contains? id contained-str)
    (string-contains? (identifier->string id)
                      contained-str))

  (define (identifier-append ctx srcloc . ids)
    (define (-> x) (datum->syntax ctx x srcloc))
    (-> (string->symbol (string-append* (map identifier->string ids)))))

  (define-syntax-class name ; an identifier without dots
    (pattern name:id
             #:when (not (identifier-contains? #'name "."))))
             
  (define-syntax-class dotted-name
    (pattern dotted-name:id
             #:attr names (identifier-split #'dotted-name "."))))
                                                
(define-syntax (import stx)
  (syntax-parse stx
    #:literals (as)
    [(_import (~seq dotted-name:dotted-name
                    (~optional (~seq as as-name:name) #:defaults ([as-name #'#f]))) ...)
     (syntax/loc stx
       (begin (import-as-name dotted-name as-name) ...))]))

(define-syntax (import-as-name stx)
  (syntax-parse stx
    #:literals (as)
    [(_import qualifier:dotted-name #f)
     (syntax/loc stx
       (begin
         (import-into-python 'qualifier)
         (define qualifier (get-obj 'qualifier))))]
    [(_import qualifier:dotted-name as-name:name)
     (syntax/loc stx
       (begin
         (import-into-python 'qualifier)
         (define as-name (get-obj 'qualifier))))]))

(define-syntax (import-from stx)
  (syntax-parse stx
    #:literals (as)
    [(_import qualifier:dotted-name
              (~seq target:dotted-name (~optional (~seq as as-name:name) #:defaults ([as-name #'#f])))
              ...)
     (syntax/loc stx
       (begin (import-from-as qualifier target as-name) ...))]))

(define-syntax (import-from-as stx)
  (syntax-parse stx
    [(_import-from qualifier:dotted-name target:name #f)
     (with-syntax ([qualifier.target (identifier-append stx stx #'qualifier "." #'target)])
       (syntax/loc stx
         (begin
           (import-into-python 'qualifier)
           (define target (get-obj 'qualifier.target)))))]
    [(_import-from qualifier:dotted-name target:name as-name:name)
     (with-syntax ([qualifier.target (identifier-append stx stx #'qualifier "." #'target)]
                   #;[as-name          (syntax-local-introduce #'as-name)])
       (syntax/loc stx
         (begin
           (import-into-python 'qualifier.target)
           (define as-name (get-obj 'qualifier.target)))))]))


;; import foo                 # foo imported and bound locally
;; import foo.bar.baz         # foo, foo.bar, and foo.bar.baz imported, foo bound locally
;; import foo.bar.baz as fbb  # foo, foo.bar, and foo.bar.baz imported, foo.bar.baz bound as fbb
;; from foo.bar import baz    # foo, foo.bar, and foo.bar.baz imported, foo.bar.baz bound as baz
;; from foo import attr       # foo imported and foo.attr bound as attr
              

;; import_stmt: import_name | import_from
;; import_name: 'import' dotted_as_names 
;; # note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
;; import_from:
;;     | 'from' ('.' | '...')* dotted_name 'import' import_from_targets 
;;     | 'from' ('.' | '...')+             'import' import_from_targets 
;; import_from_targets:
;;     | '(' import_from_as_names [','] ')' 
;;     | import_from_as_names !','
;;     | '*' 
;; import_from_as_names:
;;     | ','.import_from_as_name+ 
;; import_from_as_name:
;;     | NAME ['as' NAME ] 
;; dotted_as_names:
;;     | ','.dotted_as_name+ 
;; dotted_as_name:
;;     | dotted_name ['as' NAME ] 
;; dotted_name:
;;     | dotted_name '.' NAME 
;;     | NAME


