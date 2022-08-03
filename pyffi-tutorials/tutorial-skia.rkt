#lang racket
;;;
;;; SKIA
;;;

;; Skia is an open source 2D graphics library.
;; It is cross platform and is used in Google Chrome.
;; It's not unfair to say that Skia a modern version of Cairo
;; (Cairo is the graphics library used to implement racket/draw).

;; Skia is implemented C++ and currently there are no direct bindings
;; of Skia for Racket. However, we can use the Python bindings for Skia
;; from Racket.

(require pyffi)
(initialize)
(post-initialize)


(import skia)
(define None (void))

;;; Circle

(define (circle)
  (define width  200)
  (define height 200)
  
  (define surface (skia.Surface width height))
  

  ; (with surface as canvas : ...)
  (define canvas (surface.__enter__))
  (canvas.drawCircle 100 100 40 (skia.Paint #:Color skia.ColorRED))
  (surface.__exit__ None None None) ; None means no exceptions occured

  (define image (surface.makeImageSnapshot))
  (image.save "output-circle.png" skia.kPNG))

;;; Drop Shadow

;; canvas.drawColor(skia.ColorWHITE)
;; paint = skia.Paint(ImageFilter=skia.ImageFilters.DropShadow(3, 3, 5, 5, skia.ColorBLACK))
;; blob = skia.TextBlob("Skia", skia.Font(None, 120))
;; canvas.drawTextBlob(blob, 0, 160, paint)

(define (dropshadow)
  (define width  200)
  (define height 200)
  (define surface (skia.Surface width height))

  (define canvas (surface.__enter__))

  (canvas.drawColor skia.ColorWHITE)
  (define paint (skia.Paint #:ImageFilter (skia.ImageFilters.DropShadow 3 3 5 5 skia.ColorBLACK)))
  (define blob  (skia.TextBlob "Skia" (skia.Font None 120)))
  (canvas.drawTextBlob blob 0 160 paint)

  (surface.__exit__ None None None)
  
  (define image (surface.makeImageSnapshot))
  (image.save "output-dropshadow.png" skia.kPNG))


(circle)
(dropshadow)

