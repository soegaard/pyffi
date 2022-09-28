#lang racket/base
;; Original by Danny Yoo
;;   https://github.com/shriram/scribble-embedding/blob/master/sxml-render.rkt

;; Added html->element.

(require racket/match
         scribble/html-properties
         scribble/core
         html-parsing)


(provide html->element sxml->element)

(define (html->element x)
   (define top (cdr (html->xexp (open-input-string x))))
   (map sxml->element top))


;; sxml->element: sxml -> element
;; Embeds HTML content into a Scribble document.
(define (sxml->element an-sxml)
  (match an-sxml
    [(list '& 'nbsp)
     'nbsp]
    [(list '& sym)
     sym]

    [(list tag-name (list '@ (list attr-name attr-value) ...) children ...)
     (tagged->element tag-name attr-name attr-value children)]
    
    [(list tag-name children ...)
     (tagged->element tag-name '() '() children)]

    [(? symbol?)
     an-sxml]
    
    [(? string?)
     an-sxml]

    [(? char?)
     (string an-sxml)]))


(define (tagged->element tag-name attr-names attr-values children)
  (define tag-attr (alt-tag (symbol->string tag-name)))
  (define attrs-attr (attributes (map cons attr-names attr-values)))
  (define content (map sxml->element children))
  (make-element (make-style #f (list tag-attr attrs-attr))
                content))
