#lang racket/base
(provide (rename-out [.app #%app]))
(provide (rename-out [.top #%top]))
(provide declare-special-prefix)


(require (for-syntax racket/base racket/syntax syntax/parse racket/string racket/list))
(require (only-in "python-builtins.rkt"    getattr))
(require (only-in "python-environment.rkt" get))
(require (only-in "python-types.rkt"       pr))
(require "structs.rkt")

;; getattr(object, name[, default])

;; Return the value of the named attribute of object. Here `name` must be a
;; string. If the string is the name of one of the object’s attributes,
;; the result is the value of that attribute. For example, getattr(x,
;; 'foobar') is equivalent to x.foobar. If the named attribute does not
;; exist, default is returned if provided, otherwise AttributeError is
;; raised.

;; Instead of writing (getattr obj name #f) we want to write (.name obj).
;; Instead of defining a .name for each attribute, we are going to
;; 1. Let (#%app .name 

(define (get-obj qualified-name)
  (pr (get qualified-name)))


(begin-for-syntax
  (define (identifier->string id)
    (cond
      [(string? id)                           id]
      [(symbol? id) (symbol->string           id)]
      [else         (symbol->string (syntax-e id))]))

  (define (dot-identifier? id)
    (eqv? (string-ref (identifier->string id) 0) #\.))
  
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

  (define (identifier-begins-with? id start-ch)
    (unless (char? start-ch)
      (error 'identifier-begins-with? "expected a character as start-ch"))
    (define str (identifier->string id))
    (and (not (zero? (string-length str)))
         (eqv? (string-ref str 0) start-ch)))

  (define (method-identifier? id)
    (and (identifier-begins-with? id #\.)
         (not (identifier-contains? (identifier-drop-start id) "."))))

  (define (identifier-drop-start id)
    (define str (identifier->string id))
    (define sym (string->symbol (substring str 1 (string-length str))))
    (datum->syntax id sym id id))

  (define (identifier-append ctx srcloc . ids)
    (define (-> x) (datum->syntax ctx x srcloc))
    (-> (string->symbol (string-append* (map identifier->string ids)))))

  (define (identifier-append* ctx srcloc ids)
    (apply identifier-append ctx srcloc ids))

  (define (identifers->dotted-name id ids)
    (identifier-append* id id (add-between ids #'|.|)))

  (define (dotted-identifier->identifiers id)
    (define (-> x) (datum->syntax id x id))
    (define strs (string-split (identifier->string #'id) "."))
    (define syms (map string->symbol strs))
    (map -> syms))

  (define-syntax-class name ; an identifier without dots
    (pattern name:id
             #:when (not (identifier-contains? #'name "."))))

  (define-syntax-class method ; an identifier that begins with a dot
    (pattern name:id
             #:when (identifier-begins-with? #'name #\.)))

  (define-syntax-class non-method-id ; an identifier that does not begin with a dot
    (pattern name:id
             #:when (not (identifier-begins-with? #'name #\.))))
             
  (define-syntax-class dotted-name
    (pattern dotted-name:id
             #:when (identifier-contains? #'name ".")
             #:attr names (identifier-split #'dotted-name "."))))

(define-syntax (get-dotted stx)
  (syntax-parse stx
    [(_get-dotted id id0)
     #'id0]
    [(_get-dotted id id0 . ids)
     (define dotted-name (identifers->dotted-name #'id (syntax->list #'ids)))
     (with-syntax ([dotted-name (symbol->string (syntax-e dotted-name))])
       (syntax/loc stx
         (let ([v id0])
           (cond [(module? v) (get-obj 'id)]
                 [(obj? v)    (getattr v 'dotted-name #f)]
                 [else        id]))))]))


;;; Special Prefixes

; If a name is a special prefix 

(begin-for-syntax
  (define special-prefixes '()) ; list of symbols
  (set! special-prefixes '())

  (define (set-special-prefixes value)
    (set! special-prefixes value))
  
  (define (special? id)
    (define sym (syntax-e id))
    (and (member sym special-prefixes) #t)))

(define-syntax (declare-special-prefix stx)
  (syntax-parse stx
    [(_declare-special-prefix name ...)
     #'(begin-for-syntax
         (for ([sym (syntax->datum #'(name ...))])
           (set-special-prefixes (cons sym special-prefixes))))]))
         
         

(provide test-special)
(define-syntax (test-special stx)
  (syntax-parse stx
    [(_test-special id)
     (if (special? #'id)
         #''special
         #''not-special)]))


(define-syntax (.app stx)
  (syntax-parse stx
    [(_.app id:method e:expr (~optional default:expr #:defaults ([default #'#f])))
     #;(displayln (list 'A stx))
     (with-syntax ([name (identifier->string (identifier-drop-start #'id))])
       (syntax/loc stx
         (getattr e name default)))]

    [(_.app id:dotted-name method:method . args)
     #;(displayln (list 'B stx))
     (with-syntax ([(id0 id1 ...) (dotted-identifier->identifiers #'id)])
       (syntax/loc stx
         (let ([o (get-dotted id id0 id1 ...)])
           ((.app method o) . args))))]

    [(_.app id:dotted-name . args)
     #;(displayln (list 'C stx))
     (with-syntax ([(id0 id1 ...) (dotted-identifier->identifiers #'id)])
       (syntax/loc stx
         (let ([o (get-dotted id id0 id1 ...)])
           (o . args))))]

    [(_.app e:expr method:method . args)
     #;(displayln (list 'D stx))
     (syntax/loc stx
       ((.app method e) . args))]

    [(_.app e:expr  . args)
     #;(displayln (list 'E stx))
     (syntax/loc stx
       (e . args))]))
    
    

(define (module? x)
  (and (obj? x)
       (member (obj-type-name x) '("module" "_automodule"))
       (not (obj-the-obj x)) ; #f = null
       #t))

(define-syntax (.top stx)
  ; like #%top, but dotted identifiers are python qualified references
  (syntax-parse stx
    [(_.top . id:id)
     #;(displayln (list '.top #'id))
     (cond
       [(identifier-begins-with? #'id #\.)
        #'(#%top . id)]
       [(identifier-contains? #'id ".")
        (when (identifier-contains? #'id "..")
          (raise-syntax-error '.top "two consecutive dots not allowed" #'id))
        (with-syntax ([(str0 str1 ...) (identifier-split #'id ".")])
          (with-syntax ([id0 (datum->syntax #'id (string->symbol (syntax-e #'str0)))])
            (define (-> x) (datum->syntax stx x #'id))
            (define strs        (string-split (identifier->string #'id) "."))
            (define dotted-name (string-append* (add-between (rest strs) ".")))
            
            (with-syntax ([dotted-name dotted-name])
              (syntax/loc stx
                (let ()
                  (define v id0)
                  (cond [(module? v) (getattr v dotted-name #f)]
                        [(obj? v)    (getattr v dotted-name #f)]
                        [else        #'(#%top . id)]))))))]
       [else
        ; (displayln #'id)
        #'(#%top . id)])]))
     
