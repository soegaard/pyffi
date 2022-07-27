#lang racket/base
(require pyffi)
(require (for-syntax racket/base syntax/parse racket/syntax))

(initialize)                
(finish-initialization)


(import-into-python 'pygments)
(import-into-python 'pygments.lexers)
(import-into-python 'pygments.formatters)

(define pygments            (python->racket (get 'pygments)))
(define pygments.lexers     (python->racket (get 'pygments.lexers)))
(define pygments.formatters (python->racket (get 'pygments.formatters)))

'module?
(and (module? pygments) (module? pygments.lexers) (module? pygments.formatters))


'module-name
(and (equal? (module-name pygments)            "pygments")
     (equal? (module-name pygments.lexers)     "pygments.lexers")
     (equal? (module-name pygments.formatters) "pygments.formatters"))


(define pygments-hash            (module-dict-as-hash pygments))
(define pygments.lexers-hash     (module-dict-as-hash pygments.lexers))
(define pygments.formatters-hash (module-dict-as-hash pygments.formatters))



(module-name pygments.lexers)

;; (dict-keys   (module-dict pygments))
;; (void (dict-values (module-dict pygments)))
;; (dict-size   (module-dict pygments))
;; (dict-new)
;; (dict-proxy-new (dict-new))

(define d (module-dict pygments))
; (dict-values d)

(module-functions-with-signature pygments)
;(module-functions pygments.lexers)
;(module-functions pygments.formatters)

