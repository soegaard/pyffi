#lang racket
;;;
;;; Fish Market
;;;

;; This example uses a data set from Finland.
;; A number of fish were caught and measurements for each fish were recorded.

;;    http://jse.amstat.org/datasets/fishcatch.txt
;;    https://www.kaggle.com/datasets/aungpyaeap/fish-market

;; One way to use the data set: Consider `weight` the dependent variable
;; and try to predict the `weight` from the other data.

;;; Import and initialize Numpy

(require ; (except-in "../python.rkt" #%app #%top)
         "../python.rkt"
         "../numpy.rkt")
         
(set-environment-variables)
(initialize)                
(import-numpy)
(finish-initialization)
(declare-special-prefix numpy)


;;; 1. Load the data set

;; > less fish.csv
;; Species,Weight,Length1,Length2,Length3,Height,Width
;; Bream,242,23.2,25.4,30,11.52,4.02
;; Bream,290,24,26.3,31.2,12.48,4.3056
;; ...

;; Apart from "Species" all values are floating points.

;; >>> dtypes
;; [('Species', 'U'), ('Weight', 'f8'), ('Length1', 'f8'), ('Length2', 'f8'),
;; ('Length3', 'f8'), ('Height', 'f8'), ('Width', 'f8')]

; `dtype` stands for data type
(define dtypes (list #("Species" "U10")  ; unicode, 10 characters
                     #("Weight"  "f8")   ; floating-point
                     #("Length1" "f8")
                     #("Length2" "f8")
                     #("Length3" "f8")
                     #("Height"  "f8")
                     #("Width"   "f8")))

(define data (numpy.genfromtxt "fish.csv" #:skip_header 1 #:delimiter "," #:dtype dtypes))

;;; 2. Get to know the data set

; Print a sample of 10 fish

"Sample of 10 fish"
(define sample (numpy.random.choice data #:size 10))
sample

"Total number of fish"
; (ref (numpy.shape #:a data) 0)
(ref (numpy.shape data) 0)

"Weights in the sample"
(define sample-weights (ref sample "Weight"))
sample-weights

"Lengths in sample"
(define sample-lengths (ref sample "Length1"))
sample-lengths

"Minimal length"
(define sample-min-length (from (numpy.min sample-lengths)))
sample-min-length

"Maximal length"
(define sample-max-length (from (numpy.max sample-lengths)))
sample-max-length

; Could `weight` be estimated using `length1`?

(require plot)
(plot-new-window? #t)
(plot #:title "Sample plot" #:x-label "length1 (cm)" #:y-label "weight (g)"
      (points (list->vector (map vector (from sample-lengths) (from sample-weights)))))

; Conclusion for now: to some degree.

;;; 3. Transform data for use with Gradient Descent

(define T   numpy.transpose)
(define dot numpy.dot)

(define (column xs) ; turn an 1d array into a column vector
  (T (numpy.atleast_2d xs)))

"Weigths as column vector"
(column sample-weights)


(define (prepend-ones Xs)
  (define m (ref (numpy.shape Xs) 0))
  (numpy.hstack (vector (numpy.ones `#(,m 1))
                        (column Xs))))

"Lengths with a one-column in front"
(prepend-ones sample-lengths)


;;; 4. Find a model for `weight` based on `length1`.

;; We will attempt to find a linear model of the form:
;;    weight = w0 + w1 * length1
;; Our job is to find the weights w0 and w1.

;; If we write the model as:
;;    weight = w0*1 + w1 * length1
;; We see that the weight is the dot products between the weights and
;; the length with a prepended 1.
;;    weight = w . (1,length)   where w is the vector (w0,w1)

;; The standard regression method is minimal least squares.
;; The sum of the squares of the residual must be minimal.
;;    sum_residual_sqares(w) = sum( y - w . xs )
;; where y  is  the observed values (the weights),
;; and   xs are the independent variables with a 1 in front (here the length).


"Using Numpys least square solver"
(define sample-solution        (numpy.linalg.lstsq (prepend-ones sample-lengths) sample-weights #:rcond (void)))
(define sample-coeffs          (ref sample-solution 0))
(define sample-residuals       (ref sample-solution 1))
(define sample-rank            (ref sample-solution 2))
(define sample-singular-values (ref sample-solution 3))
"Sample: solution"
(from sample-coeffs)
"Sample: sum of squared residuals"
sample-residuals
"Sample plot with regression line"
(let ()
  (match-define (list w0 w1) (from sample-coeffs))
  (define (f x) (+ w0 (* w1 x)))
  (plot #:title "Sample with linear model" #:x-label "length1 (cm)" #:y-label "weight (g)"
        (list (points (list->vector (map vector (from sample-lengths) (from sample-weights))))
              (function f sample-min-length sample-max-length))))

;;; 5. Fine linear model using Gradient Descent

;; https://en.wikipedia.org/wiki/Gradient_descent

;; The function `sum_residual_squares` can be minimized using the method of Gradient Descent.
;;    srs(w) = sum_residual_sqares(w) = sum( y - w . xs )

;; We already know the result, but this allow us to test the validity of our grafient
;; descent implementation.

;; For a given `w` the gradient of `srs` at `w` is a vector that points in the direction
;; that maximized the growth of `srs`. Thus minus the gradient is a vector that points
;; in the direction that minimizes `srs`.

;; If we repeatedly subtract the gradient (more precisely: a vector with the same
;; direction as the gradient) from our guess, the guess will improve.

;; The idea of gradient descent is simple:

;;   w = initial_guess
;;   loop:
;;     compute gradient
;;     w = w - α gradient

;; the coefficient α is called the learning rate.
;; We will use a very simple version of gradient descent and let α be fixed.

;; The formula for the gradient of `srs` can be found at Wikipedia.

;; def descent(X, y, α = 0.001, iters = 100):
;;   w = np.zeros((X.shape[1], 1))
;;   for i in range(iters):
;;       gradient = -2.(X.T).dot(y - X.dot(w))
;;       w = w - α*gradient
;;   return w


(define (gradient-descent xss ys [α 0.001] [iterations 100])
  (define X  (prepend-ones xss))
  (set!   ys (column ys))
  (define n  (ref (numpy.shape ys) 1))          ; n = number of features measured (number of columns)
  (define w  (numpy.zeros (vector (+ n 1) 1)))  ; (n+1)x1
  
  (for ([i (in-range iterations)])
    (define predictions (dot X w))
    (define observed    ys)
    (define residuals   (sub observed predictions))
    (define gradient    (mul -2. (dot (T X) residuals)))
    (set! w (sub w (mul α gradient))))
  w)

"Sample solution found with Gradient Descent"
; these parameters seem to work for most samples
#;(gradient-descent sample-lengths sample-weights 0.0001 100000)



