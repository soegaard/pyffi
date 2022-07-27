#lang racket
(require pyffi pyffi/numpy)

;;; Conway's Game of Life

(initialize)                 ; handles `main` and `builtins`
(import-numpy)
;                            ; load the `numpy` module
;                            ; import and initialize numpy before
;                            ; running the delayed initializers
(finish-initialization)      ; run delayed setters



(define Z (numpy.array '[[0 0 0 0 0 0]
                         [0 0 0 1 0 0]
                         [0 1 0 1 0 0]
                         [0 0 1 1 0 0]
                         [0 0 0 0 0 0]
                         [0 0 0 0 0 0]]))

;; The matrix has a border consisting of zeros.
;; This makes it easier to apply the rules without worrying about special case.
;; We do not want to show the border however, so we use `show` to display the
;; the inner parts of our matrix.

(define (show A)
  ; remove outer border
  ; for each row, display it
  (for ([i (len A)])
    (displayln (numpy.ndarray.tolist (ref A i))))
  (newline))


'Z
(show Z)
'Z.T
(show (.T Z))
'Z
(show Z)


;; def compute_neigbours(Z):
;;     shape = len(Z), len(Z[0])
;;     N  = [[0,]*(shape[0])  for i in range(shape[1])]
;;     for x in range(1,shape[0]-1):
;;         for y in range(1,shape[1]-1):
;;             N[x][y] = Z[x-1][y-1]+Z[x][y-1]+Z[x+1][y-1] \
;;                     + Z[x-1][y]            +Z[x+1][y]   \
;;                     + Z[x-1][y+1]+Z[x][y+1]+Z[x+1][y+1]
;;     return N

(define (compute-neighbours Z)
  ;; (displayln (list (list 'len len)
  ;;                  (list 'numpy.array numpy.array)))
             
  (define shape (vector (len Z) (len (ref Z 0))))
  (define N (numpy.array
             (for/list ([i (ref shape 1)])
               (mul '(0) (ref shape 0)))))
  (for*/list ([x (in-range 1 (- (ref shape 0) 1))]
              [y (in-range 1 (- (ref shape 1) 1))])
    (define-values (x- x+) (values (- x 1) (+ x 1)))
    (define-values (y- y+) (values (- y 1) (+ y 1)))
    (:= N [x y] (+ (ref Z x- y-) (ref Z x  y-) (ref Z x+ y-)
                   (ref Z x- y )               (ref Z x+ y)
                   (ref Z x- y+) (ref Z x  y+) (ref Z x+ y+))))
  N)

;; ;; >>> N = np.zeros(Z.shape, dtype=int)
;; ;; >>> N[1:-1,1:-1] += (Z[ :-2, :-2] + Z[ :-2,1:-1] + Z[ :-2,2:] +
;; ;;                      Z[1:-1, :-2]                + Z[1:-1,2:] +
;; ;;                      Z[2:  , :-2] + Z[2:  ,1:-1] + Z[2:  ,2:])
;; (define (compute-neighbours/v2 Z)
;;   (define N (numpy.zeros (numpy.shape Z) #:dtype int8))
;;   N)


(show (compute-neighbours Z))

;; ;; def iterate(Z):
;; ;;     N = compute_neighbours(Z)
;; ;;     for x in range(1,shape[0]-1):
;; ;;         for y in range(1,shape[1]-1):
;; ;;              if Z[x][y] == 1 and (N[x][y] < 2 or N[x][y] > 3):
;; ;;                  Z[x][y] = 0
;; ;;              elif Z[x][y] == 0 and N[x][y] == 3:
;; ;;                  Z[x][y] = 1
;; ;;     return Z

(define (iterate Z)
  (define N (compute-neighbours Z))
  (define shape (vector (len Z) (len (ref Z 0))))
  (for* ([x (in-range 1 (- (ref shape 0) 1))]
         [y (in-range 1 (- (ref shape 1) 1))])
    (cond [(and (= (ref Z x y) 1) (or (< (ref N x y) 2) (> (ref N x y) 3)))
           (:= Z [x y] 0)]
          [(and (= (ref Z x y) 0) (= (ref N x y) 0))
           (:= Z [x y] 1)]))
  Z)

(show Z)
(void (for ([i 4]) (iterate Z)))
(show Z)

;; (show (ref Z (slice 1 3) (slice 1 3)))

;; (define B (ref Z (vector (slice 0 5) (slice 0 5))))
;; 'B
;; (show B)
;; (len B)
;; (len (ref B 0))

;; ; (slice 1 -1)
;; ;(integer->py-int 1)

;; (show (compute-neighbours/v2 Z))


