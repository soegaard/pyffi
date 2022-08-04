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


;;;
;;; DRAW
;;;

; All examples draw on a canvas and saves the result in a png file.

(define (draw f filename [width 300] [height 300])
  (define surface (skia.Surface width height))
  (define canvas  (surface.__enter__))
  (f canvas)
  (surface.__exit__ None None None)
  (define image   (surface.makeImageSnapshot))
  (image.save filename skia.kPNG))


;;;
;;; Circle
;;;

(define (circle canvas)
  (canvas.drawCircle 100 100 40 (skia.Paint #:Color skia.ColorRED)))

;;;
;;; Drop Shadow
;;;

;; canvas.drawColor(skia.ColorWHITE)
;; paint = skia.Paint(ImageFilter=skia.ImageFilters.DropShadow(3, 3, 5, 5, skia.ColorBLACK))
;; blob = skia.TextBlob("Skia", skia.Font(None, 120))
;; canvas.drawTextBlob(blob, 0, 160, paint)

(define (dropshadow canvas)
  (canvas.drawColor skia.ColorWHITE)
  (define paint (skia.Paint #:ImageFilter (skia.ImageFilters.DropShadow 3 3 5 5 skia.ColorBLACK)))
  (define blob  (skia.TextBlob "Skia" (skia.Font None 120)))
  (canvas.drawTextBlob blob 0 160 paint))

;;;
;;; Heptagram
;;;

;; void draw(SkCanvas* canvas) {
;;     const SkScalar scale = 256.0f;
;;     const SkScalar R = 0.45f * scale;
;;     const SkScalar TAU = 6.2831853f;
;;     SkPath path;
;;     path.moveTo(R, 0.0f);
;;     for (int i = 1; i < 7; ++i) {
;;         SkScalar theta = 3 * i * TAU / 7;
;;         path.lineTo(R * cos(theta), R * sin(theta));
;;     }
;;     path.close();
;;     SkPaint p;
;;     p.setAntiAlias(true);
;;     canvas->clear(SK_ColorWHITE);
;;     canvas->translate(0.5f * scale, 0.5f * scale);
;;     canvas->drawPath(path, p);
;; }

(define (heptagram canvas)
  (define scale 256.)
  (define R     (* 0.45 scale))
  (define τ     6.2831853)

  (define path (skia.Path))
  (path .moveTo R 0.)
  (for ([i (in-range 1 7)])
    (define θ (* 3/7 i τ))
    (path .lineTo (* R (cos θ)) (* R (sin θ))))
  (path .close)
  
  (define p (skia.Paint))
  (p .setAntiAlias #t)
  (canvas .drawColor skia.ColorWHITE)
  (canvas .translate (* 0.5 scale) (* 0.5 scale))
  (canvas .drawPath path p))


(draw circle      "output-circle.png")
(draw dropshadow  "output-dropshadow.png")
(draw heptagram   "output-heptagram.png")
